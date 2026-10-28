(ns clojure-algorithms.sorts.intro-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.intro-sort :refer [intro-sort intro-sort-range
                                                         insertion-sort-range
                                                         heap-sort-range
                                                         median-of-3
                                                         partition-by-pivot]]))

(def ^:private sample [4 2 6 8 1 7 8 22 14 56 27 79 23 45 14 12])
(def ^:private sample-sorted [1 2 4 6 7 8 8 12 14 14 22 23 27 45 56 79])

(deftest test-helpers
  (is (= sample-sorted (insertion-sort-range (vec sample) 0 16)))
  (is (= sample-sorted (heap-sort-range (vec sample) 0 16)))
  (is (= 12 (median-of-3 (vec sample) 0 9 15)))
  (is (= 8 (second (partition-by-pivot (vec sample) 0 16 12)))))

(deftest test-intro-sort
  (testing "doctest examples"
    (is (= sample-sorted (intro-sort sample)))
    (is (= [-44 -13 -5 -3 -1] (intro-sort [-1 -5 -3 -13 -44])))
    (is (= [] (intro-sort [])))
    (is (= [5] (intro-sort [5])))
    (is (= [-34 -7 -3 0 6 23] (intro-sort [-3 0 -7 6 23 -34])))
    (is (= [0.3 1.0 1.7 2.1 3.3] (intro-sort [1.7 1.0 3.3 2.1 0.3])))
    (is (= ["a" "b" "c" "d" "e"] (intro-sort ["d" "a" "b" "e" "c"]))))
  (testing "range sort preserves elements outside the range"
    (let [v (vec (concat [100] (range 40 0 -1) [-100]))
          expected (vec (concat [100] (range 1 41) [-100]))]
      (is (= expected (intro-sort-range v 1 41 16 0)))
      (is (= expected (intro-sort-range v 1 41 16 1)))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 200 #(rand-int 500)))]
      (is (= (vec (sort data)) (intro-sort data))))))
