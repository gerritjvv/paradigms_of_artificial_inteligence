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

(deftest test-match1
  (is (= (pattern/match (pattern/compile (pattern/line->words "I need a ?X")) (pattern/line->words "I need a vacation"))
         [{"X" "vacation"} '()]))

  (is (not (pattern/match (pattern/compile (pattern/line->words "I need a ?X")) (pattern/line->words "I really need a vacation"))))
  (is (not (pattern/match (pattern/compile (pattern/line->words "I want a ?X")) (pattern/line->words "I need a vacation"))))
  (is (= (pattern/match  (pattern/compile (pattern/line->words "I need a ?X and a ?Y")) (pattern/line->words "I need a vacation and a hobby"))
         [{"X" "vacation" "Y" "hobby"} ()])))


(deftest test-match-multi
  ;;note compile can also take a string and will run line->words on it
  (is (= (pattern/match (pattern/compile "I need a ?*X in ?Y") (pattern/line->words "I need a nice long vacation in Mallorca")))
      [{"X" ["nice" "long"] "Y" "Mallorca"} '()]))



(deftest test-and-pattern
  (let [p (pattern/compile [['?and (pattern/compile "This is a ?p") (pattern/compile "This is a ?p")]] )]
    (is (pattern/success (pattern/match p (pattern/line->words "This is a 12121"))))))


(deftest test-or-pattern
  (let [p (pattern/compile [['?or (pattern/compile "This is a ?p") (pattern/compile "The ?v is ?p")]] )]
    (is (= (pattern/match-map (pattern/match p (pattern/line->words "The number is 12121")))
           {"v" "number" "p" "12121"}))

    (is (= (pattern/match-map (pattern/match p (pattern/line->words "This is a 12121")))
           {"p" "12121"}))))


(deftest test-repeated
  (let [p (pattern/compile [['?repeat 1 5 (pattern/compile "number ?n")]])]
    (is (= (pattern/match-map (pattern/match p (pattern/line->words "number 10 number 11 number 11 end")))
           {:groups '({"n" "11"} {"n" "11"} {"n" "10"})}))))