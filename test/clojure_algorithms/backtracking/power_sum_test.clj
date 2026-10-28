(ns clojure-algorithms.backtracking.power-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.power-sum :refer [backtrack solve]]))

(deftest backtrack-test
  (is (= [0 1] (backtrack 13 2 1 0 0)))
  (is (= [0 1] (backtrack 10 2 1 0 0)))
  (is (= [0 0] (backtrack 10 3 1 0 0)))
  (is (= [0 1] (backtrack 20 2 1 0 0)))
  (is (= [0 0] (backtrack 15 10 1 0 0)))
  (is (= [0 1] (backtrack 16 2 1 0 0)))
  (is (= [0 64] (backtrack 20 1 1 0 0))))

(deftest solve-test
  (testing "doctest examples"
    (is (= 1 (solve 13 2)))
    (is (= 1 (solve 10 2)))
    (is (= 0 (solve 10 3)))
    (is (= 1 (solve 20 2)))
    (is (= 0 (solve 15 10)))
    (is (= 1 (solve 16 2))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (solve 20 1)))
    (is (thrown? clojure.lang.ExceptionInfo (solve -10 5)))))
