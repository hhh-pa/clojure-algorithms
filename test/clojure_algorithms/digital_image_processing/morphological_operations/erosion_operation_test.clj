(ns clojure-algorithms.digital-image-processing.morphological-operations.erosion-operation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.morphological-operations.erosion-operation
             :refer [rgb-to-gray gray-to-binary erosion]]))

(deftest test-rgb-to-gray
  (is (< (Math/abs (- (get-in (rgb-to-gray [[[26 255 14]]]) [0 0])
                      159.0524)) 1e-9)))

(deftest test-gray-to-binary
  (is (= [[false true false] [false true false] [false true false]]
         (gray-to-binary [[26 255 14] [5 147 20] [1 200 0]]))))

(deftest test-erosion
  (is (= [[false false false]]
         (erosion [[true true false]] [[0 1 0]])))
  (is (= [[false false false]]
         (erosion [[true false false]] [[1 1 0]])))
  (let [cross [[0 1 0] [1 1 1] [0 1 0]]
        img [[true true true] [true true true] [true true true]]]
    ;; only the centre window sums to 5 with the cross kernel
    (is (= [[false false false] [false true false] [false false false]]
           (erosion img cross)))))
