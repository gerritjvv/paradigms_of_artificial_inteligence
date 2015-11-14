(ns
  ^{:doc
    "A generic pattern matcher tools as described by Paradigms of Artificial Inteligence Programming page 179
     Note that this implementation does not follow the book's implementation but rather uses a more functional
     Clojure like approach with also several changes in notation for brevity

     Usage

      (pattern/pat-match (pattern/line->words \"I need a ?X and a ?Y\") (pattern/line->words \"I need a vacation and a hobby\")) {\"?X\" \"vacation\" \"?Y\" \"hobby\"}
      (pattern/pat-match (pattern/line->words \"I need a ?*X in ?Y\") (pattern/line->words \"I need a nice long vacation in Mallorca\")) =>   {\"?*X\" \"nice long vacation\" \"?Y\" \"Mallorca\"}

     "}
  tools.pattern
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clj-tuple :as tuple])
  (:import (clojure.lang PersistentVector ISeq Symbol Keyword)
           (java.util Collection))
  (:refer-clojure :exclude [compile]))

(declare compile-pattern)
(declare valid-pattern-schema?)
(declare ->IsConstantMergedPattern)
(declare match)

;;;;;;;;;;;;
;;;;; Public schemas


(def IsPatternSchema [(s/one s/Symbol "op") (s/one s/Any "var") (s/optional (s/pred #(or (nil? %) (fn? %))) "predicate") s/Any])

(def MatchResultSchema
  "Schema for results from the PatternBehaviour.match function [match-map/nil rest-input/nil]"
  [(s/optional (s/pred #(or (nil? %) (map? %))) "match map")
   [s/Any]])

;;;;;;;;;;;;
;;;;;;;Public utility functions
(defn line->words [line] (string/split line #"[ ]"))

;;;;;;;;;;;
;;;;;;;; private utitlity functions


(defn match-map?
  "Return the match map from a (match ..) call"
  [[match-map _]] match-map)

(defn max-coll
  "Returns the collection with the biggest count"
  [c1 c2]
  (if (> (count c1) (count c2)) c1 c2))

(defn merge-match-results
  "merge two match-results "
  [match-rs1 match-rs2]
  [(merge (first match-rs1) (first match-rs2) (max-coll (second match-rs1) (second match-rs2)) )])

(defn extract-var-name
  "Takes a string and returns based on the following rules:
   '?*var' : var
   '?var': : var
   :else :_constant_"

  [^String v]
  (cond
    (.startsWith v "?*") (.substring v 2)
    (.startsWith v "?")  (.substring v 1)
    :else :_constant_))

(defn is*-var?
  "
  v : keyword, symbol or string
  Return true if the name of v starts with ?*"
  [v]
  (.startsWith ^String (name v) "?*"))

(defn valid-multi-pattern-schema?
  "Validate patterns like ['?or ....] ['?and ....]
   do not use directly use the multimethod valid-pattern-schema?"
  [[_ & patterns]]
  (and (not-empty patterns)
       (every? valid-pattern-schema? patterns)))


(defn match-is-constant-patterns
  "Matches the constants one at a time to the input
   constants: must support nth efficiently
   Returns nil if not all matches and [] or [nextinput...] if all matches"
  [constants input]
  ;;;made ugle using nth for performance
  (let [cnt (count constants)]
    (loop [i 0 input-xs input]
      (let [x (first input-xs)]
        (if (< i cnt)
          (when x
            (let [k (nth constants i)]
              (when (= k x)
                (recur (inc i) (rest input-xs)))))
          (if x
            (cons x (rest input-xs))
            (tuple/vector)))))))

(defn constant-pattern?
  "Returns true if the pattern is a constant pattern"
  [[op var pred constant-val]]
  (= :_constant_ var))

(defn constant-pattern-val
  "Returns the constant value for a constant pattern, note does not check that the pattern
   is a constant pattern"
  [[_ _ _ constant-val]]
  constant-val)

(defn optimise-match-is-constants
  "patterns: A sequence of PatternBehaviour implementations
   return: A sequence of PatternBehaviour implementations with the '?is _constant_ paths merged"
  [patterns]
  (let [id (atom 0)
        p-f #(if (and (coll? %) (constant-pattern? %)) :_constant_ (swap! id inc))
        p-patterns (partition-by p-f patterns)

        constant-group? #(and (coll? (first %))
                              (constant-pattern? (first %)))

        optimise-f (fn [patterns2 pattern-group]

                     (if (constant-group? pattern-group)
                       (conj patterns2 ['?constants-match (into [] (map constant-pattern-val) pattern-group)])
                       (apply conj patterns2 pattern-group)))]

    (reduce optimise-f [] p-patterns)))

(defn match? [[match-map _]]
  (not (nil? match-map)))

(defn update-multi-occur-pattern
  "p: pattern
   next-p : pattern
   If next-p exist we assoc p with :terminating-predicate? #(match next-p %)"
  [p next-p]
  (if next-p
    (assoc p :terminating-predicate? #(match? (match next-p %)))
    p))

(defn update-multi-occur-patterns
  "Any MultiOccurPattern needs to have the next matcher placed as a predicate to its terminating-predicate?"
  [patterns]
  (loop [[p & p-xs] patterns acc []]
    (if p
      (recur p-xs (conj acc (if (= :multi-var (:op p)) (update-multi-occur-pattern p (first p-xs)) p)))
      acc)))

;;;;;;;;;;;
;;;;;; Internal Protocols

;;;;;;; PatternCompiler protocol supports different data types and allows plugable compilation
;;;;;;;

(defprotocol PatternCompiler
  (-compile [this] "Returns a single PatternBehavior"))

;;;;;;; Patterns are compiled into PatternBehaviour instances, this allows for fast precompiled matching
;;;;;;; To compile into behaviours multi methods are used
(defprotocol PatternBehaviour
  (-match [this inputs] "Returns [match-map restinput] where match-map can be empty or contain the variables with all their matches, if nil no match was found, inputs is a seq if inputs, restinput is the remaining input not read"))

(defrecord SingleVarPattern [var predicate?]
  PatternBehaviour
  (-match [_ inputs]
    (when-let [p (first inputs)]
      (when (predicate? p)
        (tuple/vector (tuple/hash-map var p) (rest inputs) )))))

(defrecord MultiOccurPattern [op var predicate? terminating-predicate?]
  PatternBehaviour
  (-match [_ inputs]
    (loop [acc []  input-xs inputs]
      (let [x (first input-xs)]
        (if (and (not (terminating-predicate? input-xs)) (predicate? x))
          (recur (conj acc x)
                 (rest input-xs))
          (tuple/vector (tuple/hash-map var acc) input-xs))))))


(defrecord AndPattern [matchers]
  PatternBehaviour
  (-match [_ inputs]
    ;;match each matcher over the same inputs
    ;;return the rest input from the matcher that read the farthest, and merge all maps.
    (let [vals (map #(-match % inputs) matchers)]
      (when (every? match-map? vals)
        (reduce merge-match-results vals)))))


(defrecord OrPattern [matchers]
  PatternBehaviour
  (-match [_ inputs]
    ;;return the first matcher that matches, and return its data
    ;;return the rest input from the matcher that read the farthest.
    (first (some match-map? (map #(-match % inputs) matchers)))))

(defrecord SeqPattern [matchers]
  PatternBehaviour
  ;;; match all patterns till one fails, taking the next un-used input from the previous map
  (-match [_ input]
    (let [cnt (count matchers)]
      (loop [i 0 input-xs input match-map nil]
        (if (< i cnt)
          (let [matcher (nth matchers i)]
            (when input-xs
              (let [[match-map2 rest-input] (-match matcher input-xs)]
                (if match-map2
                  (recur (inc i) rest-input (merge match-map match-map2))
                  nil))))
          (tuple/vector match-map input-xs))))))


(defrecord IsConstantMergedPattern [constants]
  PatternBehaviour
  ;;; use for optimising matching constants
  (-match [_ input]
    (let [rest-input (match-is-constant-patterns constants input)]
      (when-not (nil? rest-input)
        (tuple/vector (tuple/hash-map) rest-input)))))

;;;;;;;;; compile-to-type transforms the pattern definitions into PatternBehaviours
;;;;;;;;  a pattern definition is [
(defmulti valid-pattern-schema? "Returns true if what is passed is a valid schema" first)

(defmulti translate-to-pattern "Translate a sequence for values to a pattern the top level pattern is always an AND" type)

(defmulti compile-to-type "Return a type instance that will handle the specify behaviour for the pattern marked by a key
                           usage (compile-to-type ['?is '?x nil])"
                          (fn [pattern]
                            (first pattern)))

(defmethod valid-pattern-schema? '?is [pattern]
  (s/validate IsPatternSchema pattern))

(defmethod valid-pattern-schema? '?or [pattern]
  (valid-multi-pattern-schema? pattern))

(defmethod valid-pattern-schema? '?and [pattern]
  (valid-multi-pattern-schema? pattern))


(defmethod valid-pattern-schema? '?or [patterns]
  (and (not-empty patterns)
       (every? valid-pattern-schema? patterns)))


(defmethod translate-to-pattern String [^String v]
  (let [var-name (extract-var-name v)
        op (if (is*-var? v) '?*is '?is)]
    [op var-name (if (= :_constant_ var-name) #(zero? (= ^String v ^String %)) identity) (if (= :_constant_ var-name) v)]))

(defmethod translate-to-pattern Symbol [v]
  (let [var-name (extract-var-name (name v))
        op (if (is*-var? v) '?*is '?is)]
    [op (symbol var-name) (if (= :_constant_ var-name) #(= v %) identity) (if (= :_constant_ var-name) v)]))

(defmethod translate-to-pattern Keyword [v]
  (let [var-name (extract-var-name (name v))
        op (if (is*-var? v) '?*is '?is)]
    [op (keyword var-name) (if (= :_constant_ var-name) #(= v %) identity) (if (= :_constant_ var-name) v)]))

(defmethod translate-to-pattern PersistentVector [v] v)
(defmethod translate-to-pattern ISeq [v] v)

;;; allow pattern behviours in precompilation
(defmethod translate-to-pattern :default [v]
  (if (satisfies? PatternBehaviour v)
    v
    (throw (str "No method in multimethod 'translate-to-pattern' for dispatch value " v))))


(defmethod compile-to-type '?is [[_ var predicate? :as all]]
  (->SingleVarPattern var (if predicate? predicate? identity)))

(defmethod compile-to-type '?*is [[_ var predicate?]]
  (->MultiOccurPattern :multi-var var predicate? (fn [_] nil)))

(defmethod compile-to-type '?and [[_ & and-patterns]]
  (->AndPattern (mapv compile-pattern and-patterns)))

(defmethod compile-to-type '?or [[_ & or-patterns]]
  (->OrPattern (mapv compile-pattern or-patterns)))

(defmethod compile-to-type '?constants-match [[_ constants]]
  (->IsConstantMergedPattern constants))

(defmethod compile-to-type :default [v]
  (if (satisfies? PatternBehaviour v)
    v
    (throw (str "No method in multimethod 'compile-to-type' for dispatch value " v))))

;;TODO Test And Or -- Refactor AndPatternMatcher note that this in essence is a SequenceMatch.
;;TODO Add repeated patterns


;;;;;;;;;;;;;;;;
;;;;; public functions
(defn compile-pattern
  "The pattern must be a valid "
  [pattern]
  ;(valid-pattern-schema? pattern)
  (compile-to-type pattern))

;;;;;;;;;;;;;;;
;;;; Entry point functions
;;;; compile a pattern then match against it

(defn compile
  "Compile a grammar-input which is made of symbols, keyword, pattern short hands and patterns
   E.g
      Simple grammar [:The :?var :is :?color]
      Embedded grammar [['?is :_constant_ :The] ['?is :?var =] ['?is :_contant_ =] ['?is :?color identity]]
        Note that the embedded grammar is within a single vector i.e all grammars short hand or expanded are
        contained within a single vector"
  [grammar-input]
  (-compile grammar-input))

(defn match
  "Run a match on an input sequence using the compiled pattern
   Returns: on-success [match-map remaining-inputs] on-failure nil"
  [compiled-pattern input]
  (-match compiled-pattern input))

(defn success
  "Returns true if the match was a success"
  [match-result]
  (match? match-result))

(defn match-map
  "Return the match-map of a match result"
  [match-result]
  (first match-result))

(defn match-remaining-inputs
  "After running match there might be inputs that were not evaluated or not required for the match
   this function returns the remaining inputs from a match result"
  [match-result]
  (second match-result))

;;;; compilation extension
;;;
(extend-protocol PatternCompiler

  String
  (-compile [s]
    (-compile (line->words s)))

  Collection
  (-compile [grammar-input]
    (when (not-empty grammar-input)
      (let [matchers (->> grammar-input
                          (map translate-to-pattern)
                          optimise-match-is-constants
                          (map compile-pattern)
                          (filter (complement nil?))
                          update-multi-occur-patterns
                          (apply tuple/vector))]

        ;;for a single matcher return that matcher
        (if (= (count matchers) 1)
          (first matchers)
          (->SeqPattern matchers))))))
