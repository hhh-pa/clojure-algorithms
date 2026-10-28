(ns clojure-algorithms.computer-vision.flip-augmentation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.flip-augmentation :as f]))
(deftest flip-test
  (is (= [1 0.7 0.2 0.4 0.6] (f/flip-bbox-h [1 0.3 0.2 0.4 0.6])))
  (is (= [1 0.3 0.8 0.4 0.6] (f/flip-bbox-v [1 0.3 0.2 0.4 0.6])))
  (is (= [[1 0.7 0.2 0.4 0.6]]
         (f/flip-annos [[1 0.3 0.2 0.4 0.6]] 1)))
  (is (= 32 (count (f/random-chars 32)))))
