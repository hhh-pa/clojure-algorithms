(ns clojure-algorithms.graphs.bidirectional-search-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.bidirectional-search
             :refer [bidirectional-search]]))

(def demo {0 [1 2] 1 [0 3 4] 2 [0 5 6] 3 [1 7] 4 [1 8] 5 [2 9]
           6 [2 10] 7 [3 11] 8 [4 11] 9 [5 11] 10 [6 11] 11 [7 8 9 10]})

(deftest test-bidirectional-search
  (is (= [0 1 3 7 11] (bidirectional-search demo 0 11)))
  (is (= [5] (bidirectional-search demo 5 5)))
  (is (nil? (bidirectional-search {0 [1 2] 1 [0] 2 [0] 3 [4] 4 [3]} 0 3)))
  (is (nil? (bidirectional-search demo 0 99))))
