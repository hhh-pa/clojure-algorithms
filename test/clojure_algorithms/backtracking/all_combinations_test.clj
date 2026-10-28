(ns clojure-algorithms.backtracking.all-combinations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.all-combinations
             :refer [combination-lists generate-all-combinations create-all-state]]))

(deftest combination-lists-test
  (is (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
         (combination-lists 4 2))))

(deftest generate-all-combinations-test
  (testing "doctest examples"
    (is (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
           (generate-all-combinations 4 2)))
    (is (= [[]] (generate-all-combinations 0 0)))
    (is (= [[1 2 3 4] [1 2 3 5] [1 2 4 5] [1 3 4 5] [2 3 4 5]]
           (generate-all-combinations 5 4)))
    (is (= [[1 2 3]] (generate-all-combinations 3 3)))
    (is (= [[1] [2] [3]] (generate-all-combinations 3 1)))
    (is (= [[]] (generate-all-combinations 1 0)))
    (is (= [[1]] (generate-all-combinations 1 1))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (generate-all-combinations 10 -1)))
    (is (thrown? clojure.lang.ExceptionInfo (generate-all-combinations -1 10))))
  (testing "both generators agree"
    (is (every? true? (for [n (range 1 6) k (range 1 6)]
                        (= (generate-all-combinations n k)
                           (combination-lists n k)))))))

(deftest create-all-state-test
  (is (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
         (create-all-state 1 4 2 [])))
  (is (= [[1 2 3]] (create-all-state 1 3 3 [])))
  (is (= [[1 2]] (create-all-state 2 2 1 [1])))
  (is (= [[]] (create-all-state 1 0 0 [])))
  (is (= [[1 2]] (create-all-state 1 4 0 [1 2])))
  (is (= [] (create-all-state 5 4 2 [1 2]))))
