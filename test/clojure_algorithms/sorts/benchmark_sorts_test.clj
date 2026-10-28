(ns clojure-algorithms.sorts.benchmark-sorts-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.benchmark-sorts :refer [sorted-asc? all-sorts-agree?
                                                              benchmark]]))

(deftest test-sorted-asc?
  (is (true? (sorted-asc? [1 2 2 3])))
  (is (false? (sorted-asc? [1 3 2])))
  (is (true? (sorted-asc? []))))

(deftest test-all-sorts-agree?
  (is (true? (all-sorts-agree? [5 1 4.2 2 8.5 0 2])))
  (is (true? (all-sorts-agree? [])))
  (is (true? (all-sorts-agree? [42])))
  (is (true? (all-sorts-agree? (vec (range 5 -6 -1)))))
  (is (true? (all-sorts-agree? (vec (seq "Python"))))))

(deftest test-benchmark
  (is (thrown? IllegalArgumentException (benchmark [])))
  (is (thrown? IllegalArgumentException (benchmark [1] 0)))
  (let [timings (benchmark [3 1 2])]
    (is (= 11 (count timings)))
    (is (every? (fn [[_ t]] (>= t 0.0)) timings))))
