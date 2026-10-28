(ns clojure-algorithms.digital-image-processing.filters.median-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.median-filter
             :refer [median-filter]]))

(deftest test-median-filter
  (let [img [[1 2 3] [4 5 100] [7 8 9]]]
    ;; sorted window [1 2 3 4 5 7 8 9 100], index int(9/2+1)=5 -> 7
    (is (= [[0 0 0] [0 7 0] [0 0 0]] (median-filter img 3))))
  (let [img [[10 10 10] [10 10 10] [10 10 10]]]
    (is (= [[0 0 0] [0 10 0] [0 0 0]] (median-filter img))))
  (let [img (vec (repeat 5 (vec (range 5))))]
    (is (= 5 (count (median-filter img 3)))))
  (is (some pos? (mapcat identity (median-filter [[1 2 3] [4 5 6] [7 8 9]] 3)))))
