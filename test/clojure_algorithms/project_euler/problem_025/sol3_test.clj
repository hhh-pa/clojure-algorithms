(ns clojure-algorithms.project-euler.problem-025.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-025.sol3 :refer [solution]]))

(deftest solution-test
  (is (= 476 (solution 100)))
  (is (= 237 (solution 50)))
  (is (= 12 (solution 3))))
