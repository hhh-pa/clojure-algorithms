(ns clojure-algorithms.sorts.bubble-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bubble-sort :refer [bubble-sort-iterative
                                                          bubble-sort-recursive]]))

(def ^:private cases
  [[] [56] [0 5 2 3 2] [-2 -45 -5] [-23 0 6 -4 34]
   [1 2 3 4] [3 3 3 3] [1.1 3.3 5.5 7.7 2.2 4.4 6.6]])

(deftest test-bubble-sort-iterative
  (testing "examples"
    (is (= [0 2 2 3 5] (bubble-sort-iterative [0 5 2 3 2])))
    (is (= ["a" "b" "c" "x" "y" "z"]
           (bubble-sort-iterative ["z" "a" "y" "b" "x" "c"]))))
  (testing "matches clojure sort"
    (doseq [c cases]
      (is (= (vec (sort c)) (bubble-sort-iterative c)) (str c)))))

(deftest test-bubble-sort-recursive
  (testing "examples"
    (is (= [0 2 2 3 5] (bubble-sort-recursive [0 5 2 3 2])))
    (is (= ["A" "B" "C" "Z" "a" "c"]
           (bubble-sort-recursive ["a" "Z" "B" "C" "A" "c"]))))
  (testing "matches clojure sort"
    (doseq [c cases]
      (is (= (vec (sort c)) (bubble-sort-recursive c)) (str c)))))
