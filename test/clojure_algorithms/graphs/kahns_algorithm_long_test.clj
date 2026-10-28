(ns clojure-algorithms.graphs.kahns-algorithm-long-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.kahns-algorithm-long
             :refer [longest-distance]]))

(deftest test-kahns-long
  (is (= 5 (longest-distance {0 [2 3 4] 1 [2 7] 2 [5] 3 [5 7]
                              4 [7] 5 [6] 6 [7] 7 []})))
  (is (= 1 (longest-distance {0 [] 1 []}))))
