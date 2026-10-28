(ns clojure-algorithms.sorts.odd-even-transposition-parallel-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.odd-even-transposition-parallel
             :refer [odd-even-transposition]]))

(deftest test-odd-even-transposition
  (testing "doctest examples"
    (is (= (vec (sort (vec (range 9 -1 -1))))
           (odd-even-transposition (vec (range 9 -1 -1)))))
    (is (= ["a" "c" "x"] (odd-even-transposition ["a" "x" "c"])))
    (is (= [1.9 2.8 42.0] (odd-even-transposition [1.9 42.0 2.8])))
    (is (= [1 9 32.0] (odd-even-transposition [1 32.0 9])))
    (let [u [-442 -98 -554 266 -491 985 -53 -529 82 -429]]
      (is (= (vec (sort u)) (odd-even-transposition u)))))
  (testing "edges"
    (is (= [] (odd-even-transposition [])))
    (is (= [7] (odd-even-transposition [7])))))
