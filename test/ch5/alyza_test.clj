(ns
  ^{:doc "Tests for the alyza namespace
         "}
  ch5.alyza-test
  (:require [clojure.test :refer :all]
            [ch5.alyza :as alyza]))


(deftest test-variable
  (is (alyza/variable? "?x"))
  (is (not (alyza/variable? "x"))))

(deftest test-simple-equal
  (is (alyza/simple-equal [1 2 3] [1 2 3]))
  (is (not (alyza/simple-equal [1 2 3] [1 2 3 4 5])))
  (is (not (alyza/simple-equal [1 2 3 4 5] [1 2 3]))))

(deftest test-pat-match-simple
  (is (alyza/pat-match (alyza/line->words "I need a ?X") (alyza/line->words "I need a vacation")))
  (is (not (alyza/pat-match (alyza/line->words "I need a ?X") (alyza/line->words "I really need a vacation"))))
  (is (not (alyza/pat-match (alyza/line->words "I want a ?X") (alyza/line->words "I need a vacation"))))

  ;;check pattern bindings are returned
  (is (= (alyza/pat-match (alyza/line->words "I need a ?X and a ?Y") (alyza/line->words "I need a vacation and a hobby")) {"?X" "vacation" "?Y" "hobby"})))


(deftest test-patch-match-segment
  (is
    (=
      (alyza/pat-match (alyza/line->words "I need a ?*X in ?Y") (alyza/line->words "I need a nice long vacation in Mallorca"))
      {"?*X" "nice long vacation" "?Y" "Mallorca"}))

  (is
    (=
      (alyza/pat-match (alyza/line->words "I need a ?*X in ?Y") (alyza/line->words "I need a nice long vacation"))
      {"?*X" "nice long vacation"})))