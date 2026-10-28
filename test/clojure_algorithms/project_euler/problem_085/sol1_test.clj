(ns clojure-algorithms.project-euler.problem-085.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-085.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 6 (solution 20)))
  (is (= 72 (solution 2000)))
  (is (= 86595 (solution 2000000000))))
