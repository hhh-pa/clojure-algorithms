(ns clojure-algorithms.digital-image-processing.filters.local-binary-pattern-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.local-binary-pattern
             :refer [get-neighbors-pixel local-binary-value
                     local-binary-pattern]]))

(deftest test-lbp
  (is (nil? (get-neighbors-pixel [[1]] 0 0 nil)))
  (is (= 8 (count (get-neighbors-pixel [[1 2] [3 4]] 0 0 1))))
  ;; neighbours of 5 in [[1 2 3] [4 5 6] [7 8 9]]:
  ;; [3 6 2 9 8 7 4 1] -> bits [0 1 0 1 1 1 0 0] -> 58
  (is (= [3 6 2 9 8 7 4 1]
         (get-neighbors-pixel [[1 2 3] [4 5 6] [7 8 9]] 1 1 5)))
  (is (= 58 (local-binary-value [[1 2 3] [4 5 6] [7 8 9]] 1 1)))
  (let [flat (vec (repeat 3 (vec (repeat 3 5))))]
    ;; negative indices wrap (255s) but oversize ones yield 0
    (is (= [[255 255 244] [255 255 244] [199 199 196]]
           (local-binary-pattern flat))))
  (let [img [[10 20 30] [40 50 60] [70 80 90]]]
    (is (= 3 (count (local-binary-pattern img))))
    (is (some pos? (mapcat identity (local-binary-pattern img))))))
