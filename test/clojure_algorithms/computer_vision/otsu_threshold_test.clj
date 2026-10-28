(ns clojure-algorithms.computer-vision.otsu-threshold-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.otsu-threshold :as o]))
(deftest otsu-test
  (is (= [[0 0 0 0] [255 255 255 255] [0 0 0 0] [255 255 255 255]]
         (o/otsu-threshold [[0 0 0 0] [255 255 255 255]
                            [0 0 0 0] [255 255 255 255]]))))
