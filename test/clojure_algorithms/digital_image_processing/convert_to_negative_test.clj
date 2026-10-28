(ns clojure-algorithms.digital-image-processing.convert-to-negative-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.convert-to-negative
             :refer [convert-to-negative]]))

(deftest test-convert-to-negative
  (is (= [[[155 105 55]]] (convert-to-negative [[[100 150 200]]])))
  (is (= [[255 0]] (convert-to-negative [[0 255]])))
  (let [img [[[1 2 3] [4 5 6]]]]
    (is (= img (convert-to-negative (convert-to-negative img))))))
