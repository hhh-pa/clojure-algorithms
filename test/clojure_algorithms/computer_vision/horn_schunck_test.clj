(ns clojure-algorithms.computer-vision.horn-schunck-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.horn-schunck :as h]))
(deftest warp-test
  (is (= [[0 0 0] [3 1 0] [0 2 3]]
         (h/warp [[0 1 2] [0 3 0] [2 2 2]]
                 [[0 1 -1] [-1 0 0] [1 1 1]]
                 [[0 0 0] [0 1 0] [0 0 1]]))))
(deftest flow-test
  (let [[hf vf] (h/horn-schunck [[0 0 2] [0 0 2]] [[0 2 0] [0 2 0]] 110 0.1)]
    (is (= [2 3] [(count hf) (count (first hf))]))
    (is (= [2 3] [(count vf) (count (first vf))]))))
