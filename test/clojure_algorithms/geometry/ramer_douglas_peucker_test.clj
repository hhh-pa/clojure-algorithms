(ns clojure-algorithms.geometry.ramer-douglas-peucker-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.ramer-douglas-peucker
             :refer [ramer-douglas-peucker perpendicular-distance]]))

(deftest test-ramer-douglas-peucker
  (is (= [] (ramer-douglas-peucker [] 1.0)))
  (is (= [[0.0 0.0] [2.0 0.0]]
         (ramer-douglas-peucker [[0.0 0.0] [1.0 0.1] [2.0 0.0]] 0.5)))
  (is (= [[0.0 0.0] [1.0 1.0] [2.0 0.0]]
         (ramer-douglas-peucker [[0.0 0.0] [1.0 1.0] [2.0 0.0]] 0.5)))
  (is (thrown? IllegalArgumentException
               (ramer-douglas-peucker [[0.0 0.0] [1.0 0.5] [2.0 0.0]] -1.0)))
  (is (= 4.0 (perpendicular-distance [4.0 0.0] [0.0 0.0] [0.0 3.0]))))
