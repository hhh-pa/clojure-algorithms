(ns clojure-algorithms.computer-vision.mean-threshold-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.mean-threshold :as m]))
(deftest mean-test
  (is (= [[0 0] [255 255]] (m/mean-threshold [[10 20] [200 210]]))))
