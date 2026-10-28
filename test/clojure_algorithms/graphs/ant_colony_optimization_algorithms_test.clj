(ns clojure-algorithms.graphs.ant-colony-optimization-algorithms-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.ant-colony-optimization-algorithms
             :refer [distance pheromone-update ant-colony-tsp]]))

(deftest test-ant-colony
  (is (= 5.0 (distance [0 0] [3 4])))
  (is (= 5.0 (distance [0 0] [-3 -4])))
  (let [[ph bp bd] (pheromone-update [[1.0 1.0] [1.0 1.0]]
                                     {0 [0 0] 1 [2 2]} 0.7
                                     [[0 1 0]] 10 [] Double/POSITIVE_INFINITY)]
    (is (< (Math/abs (- (get-in ph [0 0]) 0.7)) 1e-9))
    (is (< (Math/abs (- (get-in ph [0 1]) 4.235533905932737)) 1e-9))
    (is (= [0 1 0] bp))
    (is (< (Math/abs (- bd 5.656854249492381)) 1e-9)))
  (let [[path dist] (ant-colony-tsp {0 [0 0] 1 [2 2]} 5 5 0.7 1.0 5.0 10)]
    (is (= [0 1 0] path))
    (is (< (Math/abs (- dist 5.656854249492381)) 1e-9))))
