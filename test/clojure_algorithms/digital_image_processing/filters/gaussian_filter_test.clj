(ns clojure-algorithms.digital-image-processing.filters.gaussian-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.gaussian-filter
             :refer [gen-gaussian-kernel gaussian-filter]]))

(deftest test-gaussian-kernel
  (let [k (gen-gaussian-kernel 3 1)]
    (is (= 3 (count k) (count (first k))))
    (is (< (Math/abs (- (get-in k [1 1]) (/ 1 (* 2 Math/PI)))) 1e-12))
    (is (= (nth (nth k 0) 0) (nth (nth k 0) 2)
           (nth (nth k 2) 0) (nth (nth k 2) 2))))
  (is (every? pos? (mapcat identity (gen-gaussian-kernel 9 1.4)))))

(deftest test-gaussian-filter
  (let [img (vec (repeat 5 (vec (repeat 5 100))))
        out (gaussian-filter img 3 1)]
    (is (= 3 (count out) (count (first out))))
    (is (every? #(= 77 %) (mapcat identity out))))
  (let [img (vec (repeat 7 (vec (repeat 7 200))))
        out (gaussian-filter img 5 0.8)]
    (is (= 3 (count out)))
    (is (every? #(= (first (first out)) %) (mapcat identity out)))))
