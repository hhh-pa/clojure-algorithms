(ns clojure-algorithms.dynamic-programming.rod-cutting-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.rod-cutting :as rc]))

(deftest rod-cutting-test
  (testing "maximum revenues"
    (is (= 10 (rc/naive-cut-rod-recursive 4 [1 5 8 9])))
    (is (= 30 (rc/naive-cut-rod-recursive 10 [1 5 8 9 10 17 17 20 24 30])))
    (is (= 10 (rc/top-down-cut-rod 4 [1 5 8 9])))
    (is (= 30 (rc/top-down-cut-rod 10 [1 5 8 9 10 17 17 20 24 30])))
    (is (= 10 (rc/bottom-up-cut-rod 4 [1 5 8 9])))
    (is (= 30 (rc/bottom-up-cut-rod 10 [1 5 8 9 10 17 17 20 24 30])))
    (is (= 36 (rc/bottom-up-cut-rod 6 [6 10 12 15 20 23])))
    (is (= 36 (rc/top-down-cut-rod 6 [6 10 12 15 20 23])))
    (is (= 36 (rc/naive-cut-rod-recursive 6 [6 10 12 15 20 23]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (rc/bottom-up-cut-rod -1 [1 2])))
    (is (thrown? IllegalArgumentException (rc/bottom-up-cut-rod 3 [1 2])))))
