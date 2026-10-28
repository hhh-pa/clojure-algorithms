(ns clojure-algorithms.computer-vision.pooling-functions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.pooling-functions :as p]))
(deftest pooling-test
  (is (= [[6 8] [14 16]]
         (p/maxpooling [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]] 2 2)))
  (is (= [[241 180] [241 157]]
         (p/maxpooling [[147 180 122] [241 76 32] [126 13 157]] 2 1)))
  (is (= [[3.0 5.0] [11.0 13.0]]
         (p/avgpooling [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]] 2 2)))
  (is (= [[161.0 102.0] [114.0 69.0]]
         (p/avgpooling [[147 180 122] [241 76 32] [126 13 157]] 2 1)))
  (is (thrown? IllegalArgumentException (p/maxpooling [[1 2 3] [4 5 6]] 2 1))))
