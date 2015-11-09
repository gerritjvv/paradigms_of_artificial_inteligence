(ns
  ^{:doc "Tests for the pattern namespace
         "}
  tools.pattern-test
  (:require [clojure.test :refer :all]
            [tools.pattern :as pattern]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test utility functions

(deftest test-extract-var
  (is (= (pattern/extract-var-name "?*var") "var"))
  (is (= (pattern/extract-var-name "?var") "var"))
  (is (= (pattern/extract-var-name "var")  :_constant_)))


(deftest test-match-is-constant-patterns
  (is (= (pattern/match-is-constant-patterns ["a" "b" "c"] ["a" "b" "c" "d" "e"]) ["d" "e"]))
  (is (not (pattern/match-is-constant-patterns ["a" "b" "c"] ["a" "2" "c" "d"])))
  (is (not (pattern/match-is-constant-patterns ["a" "b" "c"] ["a" "2"]))))


(deftest test-optimise-match-is-constants
  (let [[op vals] (first (pattern/optimise-match-is-constants (map pattern/translate-to-pattern ["the" "dog" "is" "?color"])))]
         (is (= '?constants-match op))
         (is (= ["the" "dog" "is"] vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test translate
(deftest translate-symbol
  (is 'var (second (pattern/translate-to-pattern '?var))))

(deftest translate-symbol-multi
  (is 'var (second (pattern/translate-to-pattern '?var))))

(deftest translate-keyword
  (is :var (second (pattern/translate-to-pattern :?var))))

(deftest translate-keyword-multi
  (is :var (second (pattern/translate-to-pattern :?*var))))

(deftest translate-string
  (is "var" (second (pattern/translate-to-pattern "?var"))))

(deftest translate-string
  (is "var" (second (pattern/translate-to-pattern "?*var"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test pattern compiles

(deftest compile-is-pattern
  (is (satisfies? pattern/PatternBehaviour (pattern/compile-pattern ['?is :a identity]))))

(deftest compile-or-pattern
  (is (satisfies? pattern/PatternBehaviour (pattern/compile-pattern ['?or ['?is :a identity ] ['?is :a number?]]))))

(deftest compile-or-pattern
  (is (satisfies? pattern/PatternBehaviour (pattern/compile-pattern ['?and ['?is :a identity] ['?is :a number?]]))))

(defn test-match1 []
  (is (= (pattern/match (pattern/compile (pattern/line->words "I need a ?X")) (pattern/line->words "I need a vacation"))
         {"X" "vacation"}))

  (is (not (pattern/match (pattern/compile (pattern/line->words "I need a ?X")) (pattern/line->words "I really need a vacation"))))
  (is (not (pattern/match (pattern/compile (pattern/line->words "I want a ?X")) (pattern/line->words "I need a vacation"))))
  (is (= (pattern/match   (pattern/compile (pattern/line->words "I need a ?X and a ?Y")) (pattern/line->words "I need a vacation and a hobby")) {"X" "vacation" "Y" "hobby"})))


(defn test-match-multi []
  (is (= (pattern/match   (pattern/compile (pattern/line->words "I need a ?*X in ?Y")) (pattern/line->words "I need a nice long vacation in Mallorca")))
      {"X" ["nice" "long"] "Y" "Mallorca"}))

(comment
  (deftest test-variable
    (is (pattern/variable? "?x"))
    (is (not (pattern/variable? "x"))))

  (deftest test-pat-match-simple
    (is (pattern/pat-match (pattern/line->words "I need a ?X") (pattern/line->words "I need a vacation")))
    (is (not (pattern/pat-match (pattern/line->words "I need a ?X") (pattern/line->words "I really need a vacation"))))
    (is (not (pattern/pat-match (pattern/line->words "I want a ?X") (pattern/line->words "I need a vacation"))))

    ;;check pattern bindings are returned
    (is (= (pattern/pat-match (pattern/line->words "I need a ?X and a ?Y") (pattern/line->words "I need a vacation and a hobby")) {"?X" "vacation" "?Y" "hobby"})))


  (deftest test-patch-match-segment
    (is
      (=
        (pattern/pat-match (pattern/line->words "I need a ?*X in ?Y") (pattern/line->words "I need a nice long vacation in Mallorca"))
        {"?*X" "nice long vacation" "?Y" "Mallorca"}))

    (is
      (=
        (pattern/pat-match (pattern/line->words "I need a ?*X in ?Y") (pattern/line->words "I need a nice long vacation"))
        {"?*X" "nice long vacation"})))
  )