(ns clojure-algorithms.graphs.greedy-min-vertex-cover-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.greedy-min-vertex-cover
             :refer [greedy-min-vertex-cover]]))

(deftest test-greedy-min-vertex-cover
  (is (= #{0 1 2 4}
         (greedy-min-vertex-cover {0 [1 3] 1 [0 3] 2 [0 3 4]
                                   3 [0 1 2] 4 [2 3]})))
  (is (= #{} (greedy-min-vertex-cover {0 [] 1 []}))))
