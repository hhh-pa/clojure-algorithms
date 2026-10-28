(ns clojure-algorithms.project-euler.problem-191.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-191.sol1 :refer [calculate
                                                                       solution]]))

(deftest calculate-test
  (is (= 43 (calculate 4 0 0)))
  (is (= 0 (calculate 30 2 0)))
  (is (= 98950096 (calculate 30 1 0))))

(deftest solution-test
  (is (= 1918080160 (solution)))
  (is (= 43 (solution 4))))
