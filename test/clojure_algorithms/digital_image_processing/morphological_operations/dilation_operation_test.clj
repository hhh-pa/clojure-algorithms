(ns clojure-algorithms.digital-image-processing.morphological-operations.dilation-operation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.morphological-operations.dilation-operation
             :refer [rgb-to-gray gray-to-binary dilation]]))

(deftest test-rgb-to-gray
  (is (< (Math/abs (- (get-in (rgb-to-gray [[[127 255 0]]]) [0 0])
                      187.6453)) 1e-9))
  (is (= [[0.0]] (rgb-to-gray [[[0 0 0]]])))
  (is (< (Math/abs (- (get-in (rgb-to-gray [[[2 4 1]]]) [0 0]) 3.0598)) 1e-9)))

(deftest test-gray-to-binary
  (is (= [[false true false]] (gray-to-binary [[127 255 0]])))
  (is (= [[false]] (gray-to-binary [[0]])))
  (is (= [[false false false]] (gray-to-binary [[26.2409 4.9315 1.4729]]))))

(deftest test-dilation
  (is (= [[false false false]]
         (dilation [[true false true]] [[0 1 0]])))
  (is (= [[false false false]]
         (dilation [[false false true]] [[1 0 1]])))
  (let [cross [[0 1 0] [1 1 1] [0 1 0]]
        img [[false false false] [false true false] [false false false]]]
    (is (= [[false true false] [true true true] [false true false]]
           (dilation img cross)))))
