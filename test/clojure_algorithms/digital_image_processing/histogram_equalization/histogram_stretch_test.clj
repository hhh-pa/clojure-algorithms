(ns clojure-algorithms.digital-image-processing.histogram-equalization.histogram-stretch-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.histogram-equalization.histogram-stretch
             :refer [equalization-mapping histogram-stretch]]))

(deftest test-histogram-stretch
  (let [img [[0 0] [255 255]]]
    (is (= [127 255] [(nth (equalization-mapping img) 0)
                      (nth (equalization-mapping img) 255)]))
    (is (= [[127 127] [255 255]] (histogram-stretch img))))
  (let [img [[0 128 255]]]
    (is (= [[85 170 255]] (histogram-stretch img))))
  (let [img [[5 5] [5 5]]]
    ;; a single grey level maps to int(255 * 1.0)
    (is (= [[255 255] [255 255]] (histogram-stretch img)))))
