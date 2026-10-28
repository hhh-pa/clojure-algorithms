(ns clojure-algorithms.data-structures.kd-tree.nearest-neighbour-search-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.kd-tree.build-kdtree
             :refer [build-kdtree]]
            [clojure-algorithms.data-structures.kd-tree.nearest-neighbour-search
             :refer [nearest-neighbour-search]]))

(defn- brute-nearest [points query]
  (apply min-key (fn [p] (reduce + (map (fn [x y] (let [d (- x y)] (* d d)))
                                        query p)))
         points))

(deftest test-nearest-neighbour
  (let [points [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]]
        tree (build-kdtree points)
        query [9 2]
        [nearest dist visited] (nearest-neighbour-search tree query)]
    (is (= (brute-nearest points query) nearest))
    (is (= (reduce + (map (fn [x y] (let [d (- x y)] (* d d)))
                          query nearest))
           dist))
    (is (pos? visited)))
  (let [points (mapv (fn [i] [(* i 1.0) (mod (* i i) 7.0)]) (range 20))
        tree (build-kdtree points)]
    (doseq [q [[0.5 0.5] [10.0 3.0] [19.0 0.0]]]
      (let [[nearest dist _] (nearest-neighbour-search tree q)]
        (is (= (brute-nearest points q) nearest))
        (is (>= dist 0)))))
  (let [[nearest dist visited]
        (nearest-neighbour-search (build-kdtree []) [0.0 0.0])]
    (is (nil? nearest))
    (is (= ##Inf dist))
    (is (= 0 visited))))
