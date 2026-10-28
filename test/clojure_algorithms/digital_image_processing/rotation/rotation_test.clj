(ns clojure-algorithms.digital-image-processing.rotation.rotation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.rotation.rotation
             :refer [get-affine-transform warp-affine get-rotation]]))

(deftest test-affine-transform
  (let [m (get-affine-transform [[0 0] [1 0] [0 1]] [[0 0] [1 0] [0 1]])]
    (doseq [[got want] (map vector (mapcat identity m) [1 0 0 0 1 0])]
      (is (< (Math/abs (- got want)) 1e-9))))
  (let [m (get-affine-transform [[0 0] [1 0] [0 1]] [[1 1] [2 1] [1 2]])]
    (doseq [[got want] (map vector (mapcat identity m) [1 0 1 0 1 1])]
      (is (< (Math/abs (- got want)) 1e-9)))))

(deftest test-warp-identity
  (let [img [[1 2 3] [4 5 6] [7 8 9]]]
    (is (= img (warp-affine img [[1 0 0] [0 1 0]] 3 3)))))

(deftest test-get-rotation
  (let [img [[1 2 3] [4 5 6] [7 8 9]]
        moved (get-rotation img [[0 0] [2 0] [0 2]] [[1 1] [3 1] [1 3]] 3 3)]
    ;; content shifted down-right by 1; top row/left col read out-of-bounds
    (is (= [[0 0 0] [0 1 2] [0 4 5]] moved))
    (is (= img (get-rotation img [[0 0] [1 0] [0 1]]
                             [[0 0] [1 0] [0 1]] 3 3)))))
