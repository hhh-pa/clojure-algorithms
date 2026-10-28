(ns clojure-algorithms.graphs.edmonds-blossom-algorithm-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.edmonds-blossom-algorithm
             :refer [maximum-matching]]))

(defn- normalize [matching]
  (vec (sort-by first (mapv #(vec (sort %)) matching))))

(deftest test-maximum-matching
  (is (= [[0 1]] (normalize (maximum-matching [[0 1] [1 2] [2 0]] 3))))
  (is (= [[0 1] [3 4]]
         (normalize (maximum-matching [[0 1] [1 2] [3 4]] 5))))
  (is (= [[0 1] [2 3] [4 5]]
         (normalize (maximum-matching [[0 1] [1 2] [2 3] [3 0] [4 5]] 6))))
  (is (= [] (maximum-matching [] 3)))
  (let [edges [[0 1] [1 2] [2 3] [3 4] [4 5] [5 0] [1 4] [2 5]]
        m (normalize (maximum-matching edges 6))
        eset (set (map set edges))]
    (is (= 3 (count m)))
    (is (contains? #{[[0 1] [2 5] [3 4]] [[0 1] [2 3] [4 5]]} m))
    (doseq [[a b] m]
      (is (contains? eset #{a b})))
    (is (= (* 2 (count m)) (count (distinct (apply concat m)))))))
