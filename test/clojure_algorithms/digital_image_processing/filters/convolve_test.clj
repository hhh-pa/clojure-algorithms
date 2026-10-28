(ns clojure-algorithms.digital-image-processing.filters.convolve-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.convolve
             :refer [img-convolve]]))

(deftest test-img-convolve
  (let [img [[1 2 3] [4 5 6] [7 8 9]]
        identity-kernel [[0 0 0] [0 1 0] [0 0 0]]]
    (is (= img (img-convolve img identity-kernel))))
  (let [img [[5 5 5] [5 5 5] [5 5 5]]
        laplacian [[0 1 0] [1 -4 1] [0 1 0]]]
    (is (= [[0 0 0] [0 0 0] [0 0 0]]
           (img-convolve img laplacian))))
  (let [img [[1 2] [3 4]]
        k [[1 0 0] [0 0 0] [0 0 0]]]
    (is (= [[1 1] [1 1]] (img-convolve img k)))))
