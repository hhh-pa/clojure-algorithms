(ns clojure-algorithms.digital-image-processing.change-contrast-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.change-contrast
             :refer [contrast-factor change-contrast]]))

(deftest test-change-contrast
  (is (= 1.0 (contrast-factor 0)))
  (let [img [[[100 100 100]]]]
    (is (= [[[100 100 100]]] (change-contrast img 0))))
  (is (= 58 (get-in (change-contrast [[[100]]] 110) [0 0 0])))
  (is (= [[255]] (change-contrast [[200]] 170)))
  (is (= [[0]] (change-contrast [[50]] 170))))
