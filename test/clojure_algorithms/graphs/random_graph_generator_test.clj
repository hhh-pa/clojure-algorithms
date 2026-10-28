(ns clojure-algorithms.graphs.random-graph-generator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.random-graph-generator
             :refer [random-graph complete-graph]]))

(deftest test-random-graph-generator
  (is (= {0 [1 2] 1 [0 2] 2 [0 1]} (complete-graph 3)))
  (is (= (complete-graph 4) (random-graph 4 1)))
  (is (= {0 [] 1 [] 2 [] 3 []} (random-graph 4 0)))
  (let [g (random-graph 6 0.5)]
    (is (= (set (range 6)) (set (keys g))))
    (doseq [[u vs] g v vs]
      (is (some #{u} (get g v))))))
