(ns
  ^{:doc "
  Alyza implementation in Clojure

  Variables ?X is a variable
  Segment variables ?*X is a segment variable
     match \"I ?*X house\" \"I have a big house\" == {\"?*X\" \"have a big\"}
  "}
  ch5.alyza
  (:require [clojure.string :as string]
            [schema.core :as s]))


; read . match . output

;;;;;;;;;;;;;;;;
;;; Private api


(defn stdin-line-reader [] (.readLine (System/console)))

(defn stdout-writer [txt] (prn txt))

(defn line->words [line] (string/split line #"[ ]"))
(defn one-word [s] (string/join \space s))

(defn variable?
  "True if the string starts with a ? char"
  [x]
  (and (string? x)
       (= (first (str x)) \?)))

(defn segment-variable?
  "True if the string starts with ?*"
  [x]
  (and (string? x)
       (= (first x) \?)
       (= (second x) \*)))

(defn simple-equal
  "Note that clojure uses = which can compare strings in sequences etc"
  [x y]
  (when (= (count x) (count y))
    (loop [xi x yi y]
      (if-let [a (first xi)]
        (when (= a (first yi))
          (recur (rest xi) (rest yi)))
        true))))

(defn variable-or-equal?
  "If a variable returns [var binding],
   else if x = y returns []
   else return FAIL"
  [[x y]]
  (cond
    (variable? x) [x y]
    (= x y)       []
    :else         nil))


(defn take-while'
  "A take while that returns [ pred-list rest ]
   This operation is not lazy"
  [pred? s]
  {:pre [(fn? pred?) (seq? s)]}
  (loop [acc [] s' s]
    (let [[x & xs] s']
      (if (and x (pred? x))
        (recur (conj acc x) xs)
        [acc s']))))

(defn segment-aware-interleave
  "Take pattern, input and if perform an interleave,
   if pattern[n] starts with '?*' then read the next pattern and slurp the input into
   a single token till end of input or input is equal to the next pattern"
  [[x & xs] input]
  (when (and x input)
    (if (segment-variable? x)
      (let [[segment ys'] (take-while' #(not= % (first xs)) input)]
        (cons x (cons (one-word segment) (segment-aware-interleave xs ys'))))
      (cons x (cons (first input) (segment-aware-interleave xs (rest input)))))))

(s/defn pat-match :- {s/Str s/Str}
  "Partially matches the pattern to the input.
   Note that the input maybe longer than the pattern and the pattern will still match
    i.e its more of a starts-with than equal
    Returns nil if no match was found, and an empty map or a map with {var var-value} bindings if any bindings in the pat match exist"
  [pattern :- [s/Str] input :- [s/Str]]
  (let [match-res (map variable-or-equal? (partition 2 (segment-aware-interleave pattern input)))]
    (when (every? (complement nil?) match-res)
      (into {} (filter not-empty match-res)))))

(defn read-match-loop
  "Loop that will read a line from readre match and and then send the output to the writer"
  [line-reader
   matcher-f
   writer
   & {:keys [pause-f] :or {pause-f #(Thread/sleep 1000)}}]
  {:pre [(fn? line-reader) (fn? matcher-f) (fn? writer)]}
  (loop []
    (when-let [line (line-reader)]
      (writer (matcher-f line))
      (pause-f)
      (recur))))



;;;;;;;;;;;;;;
;;;Public API

