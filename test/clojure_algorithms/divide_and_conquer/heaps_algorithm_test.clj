(ns clojure-algorithms.divide-and-conquer.heaps-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.heaps-algorithm :as h]))

(deftest heaps-test
  (testing "recursive Heap's algorithm"
    (is (= [[]] (h/heaps [])))
    (is (= [[0]] (h/heaps [0])))
    (is (= [[-1 1] [1 -1]] (h/heaps [-1 1])))
    (is (= [[1 2 3] [2 1 3] [3 1 2] [1 3 2] [2 3 1] [3 2 1]]
           (h/heaps [1 2 3])))
    (is (= 24 (count (h/heaps [1 2 3 4]))))
    (is (= 24 (count (set (h/heaps [1 2 3 4])))))))
