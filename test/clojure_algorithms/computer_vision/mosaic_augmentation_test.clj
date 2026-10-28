(ns clojure-algorithms.computer-vision.mosaic-augmentation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.mosaic-augmentation :as m]))
(deftest mosaic-test
  (let [annos (m/mosaic-annos [[[0 0.5 0.5 1.0 1.0]] [] [] []] [0.5 0.5])]
    (is (= [[0 0.25 0.25 0.5 0.5]] annos)))
  (is (= [360 640] (m/mosaic-quadrant-split [720 1280] [0.5 0.5])))
  (is (= [[0 0.1 0.1 0.5 0.5]]
         (m/filter-tiny [[0 0.1 0.1 0.5 0.5] [1 0.0 0.0 0.001 0.002]] 0.01)))
  (is (= 32 (count (m/random-chars 32)))))
