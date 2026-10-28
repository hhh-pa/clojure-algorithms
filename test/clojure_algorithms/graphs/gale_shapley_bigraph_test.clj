(ns clojure-algorithms.graphs.gale-shapley-bigraph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.gale-shapley-bigraph
             :refer [stable-matching]]))

(deftest test-stable-matching
  (is (= [1 2 3 0]
         (stable-matching [[0 1 3 2] [0 2 3 1] [1 0 2 3] [0 3 1 2]]
                          [[3 1 2 0] [3 1 0 2] [0 3 1 2] [1 0 3 2]])))
  (is (= [0] (stable-matching [[0]] [[0]]))))
