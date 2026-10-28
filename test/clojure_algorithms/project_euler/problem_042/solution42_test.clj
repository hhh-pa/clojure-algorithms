(ns clojure-algorithms.project-euler.problem-042.solution42-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-042.solution42 :refer [word-value triangle-word? solution triangular-numbers]]))

(deftest triangular-numbers-test
  (is (= [1 3 6 10 15 21 28 36 45 55] (take 10 triangular-numbers))))

(deftest word-value-test
  (is (= 55 (word-value "SKY")))
  (is (= 1 (word-value "A"))))

(deftest triangle-word?-test
  (is (true? (triangle-word? "SKY")))
  (is (false? (triangle-word? "B"))))

(deftest solution-test
  (is (= 2 (solution "\"SKY\",\"A\",\"B\""))))
