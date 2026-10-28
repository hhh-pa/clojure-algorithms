(ns clojure-algorithms.computer-vision.intensity-based-segmentation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.intensity-based-segmentation :as s]))
(deftest segment-test
  (is (= [[1 2 3] [0 1 2] [0 1 1]]
         (s/segment-image [[80 120 180] [40 90 150] [20 60 100]] [50 100 150]))))
