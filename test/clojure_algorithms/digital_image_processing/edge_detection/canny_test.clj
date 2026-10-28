(ns clojure-algorithms.digital-image-processing.edge-detection.canny-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.edge-detection.canny
             :refer [gen-gaussian-kernel suppress-non-maximum
                     detect-high-low-threshold track-edge canny]]))

(deftest test-gen-gaussian-kernel
  (let [k (gen-gaussian-kernel 9 1.4)]
    (is (= 9 (count k) (count (first k))))
    (is (every? pos? (mapcat identity k)))))

(deftest test-suppress-non-maximum
  (let [shape [3 3]
        dir [[0 0 0] [0 90 0] [0 0 0]]
        grad [[1 2 3] [4 9 5] [6 7 8]]
        out (suppress-non-maximum shape dir grad)]
    ;; direction 90 -> vertical neighbours 2 and 7; 9 wins
    (is (= 9 (get-in out [1 1])))
    (is (zero? (get-in out [0 0])))))

(deftest test-threshold-and-track
  (let [shape [4 4]
        dest [[0 0 0 0] [0 40 20 0] [0 10 255 0] [0 0 0 0]]
        th (detect-high-low-threshold shape dest 15 30 128 255)]
    (is (= 255 (get-in th [1 1])))
    (is (= 128 (get-in th [1 2])))
    (is (zero? (get-in th [2 1]))))
  (let [shape [4 4]
        dest [[0 0 0 0] [0 128 0 0] [0 255 0 0] [0 0 0 0]]]
    (is (= 255 (get-in (track-edge shape dest 128 255) [1 1]))))
  (let [shape [4 4]
        dest [[0 0 0 0] [0 128 0 0] [0 0 0 0] [0 0 0 0]]]
    (is (zero? (get-in (track-edge shape dest 128 255) [1 1])))))

(deftest test-canny
  (let [img (mapv (fn [_] (vec (concat (repeat 8 0) (repeat 8 255))))
                  (range 16))
        out (canny img)]
    (is (= 16 (count out) (count (first out))))
    (is (some #(= 255 %) (mapcat identity out)))))
