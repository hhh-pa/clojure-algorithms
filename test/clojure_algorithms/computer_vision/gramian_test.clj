(ns clojure-algorithms.computer-vision.gramian-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.gramian :as g]))
(defn- approx [a b] (< (Math/abs (double (- a b))) 1e-9))
(deftest gramian-test
  (let [ones2 (vec (repeat 2 (vec (repeat 5 (vec (repeat 5 1))))))]
    (is (= [[0.5 0.5] [0.5 0.5]] (g/gram-matrix ones2))))
  (let [ones3 (vec (repeat 3 (vec (repeat 5 (vec (repeat 5 1))))))]
    (is (= [3 3] [(count (g/gram-matrix ones3))
                  (count (first (g/gram-matrix ones3)))]))
    (is (approx 1.0 (g/gram-loss (vec (repeat 3 (vec (repeat 5 (vec (repeat 5 0))))))
                                 ones3))))
  (let [a [[[1 2] [3 4]]]]
    (is (= 0.0 (g/gram-loss a a)))))
