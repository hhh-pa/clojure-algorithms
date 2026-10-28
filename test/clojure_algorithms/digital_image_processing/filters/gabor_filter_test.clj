(ns clojure-algorithms.digital-image-processing.filters.gabor-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.gabor-filter
             :refer [gabor-filter-kernel]]))

(deftest test-gabor-kernel
  (let [k (gabor-filter-kernel 3 8 0 10 0 0)]
    (doseq [row k]
      (is (< (Math/abs (- (nth row 0) 0.8027212)) 1e-6))
      (is (= 1.0 (nth row 1)))
      (is (< (Math/abs (- (nth row 2) 0.8027212)) 1e-6))))
  (let [k (gabor-filter-kernel 10 8 30 10 0 0)]
    (is (= 11 (count k) (count (first k)))))
  (let [k (gabor-filter-kernel 5 8 90 10 0 0)]
    (is (every? #(Double/isFinite (double %)) (mapcat identity k)))
    (is (= 5 (count k)))))
