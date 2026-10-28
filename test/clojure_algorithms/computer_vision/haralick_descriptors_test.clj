(ns clojure-algorithms.computer-vision.haralick-descriptors-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.haralick-descriptors :as h]))
(defn- approx [a b] (< (Math/abs (double (- a b))) 1e-9))
(deftest haralick-test
  (is (= 0.0 (h/root-mean-square-error [1 2 3] [1 2 3])))
  (is (approx 0.816496580927726 (h/root-mean-square-error [1 2 3] [2 2 2])))
  (is (= [0.0 0.2 0.6 1.0] (vec (h/normalize-array [2 3 5 7]))))
  (is (= [[158 97] [56 200]]
         (h/grayscale [[[108 201 72] [255 11 127]] [[56 56 56] [128 255 107]]])))
  (is (= [[1 1] [0 1]] (h/binarize [[128 255] [101 156]])))
  (let [img (h/binarize [[1 0.5] [0.2 0.7]] 0.5)]
    (is (= [[1 1] [1 1]] (h/transform img "erosion")))
    (is (= [[0 0] [0 0]] (h/transform img "dilation")))
    (is (= [[1 1] [1 1]] (h/opening-filter img)))
    (is (= [[0 0] [0 0]] (h/closing-filter img))))
  (is (= [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
         (h/haralick-descriptors [[0.0 0.0] [0.0 0.0]])))
  (is (approx 3.3166247903554 (h/euclidean [1 0 -2] [2 -1 1]))))
