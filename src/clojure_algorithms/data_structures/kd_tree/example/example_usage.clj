(ns clojure-algorithms.data-structures.kd-tree.example.example-usage
  "Port of data_structures/kd_tree/example/example_usage.py.
  Demo: build a KD-tree over random hypercube points and run a
  nearest-neighbour query. Returns a result map (no printing)."
  (:require [clojure-algorithms.data-structures.kd-tree.build-kdtree
             :refer [build-kdtree]]
            [clojure-algorithms.data-structures.kd-tree.example.hypercube-points
             :refer [hypercube-points]]
            [clojure-algorithms.data-structures.kd-tree.nearest-neighbour-search
             :refer [nearest-neighbour-search]]))

(defn run-demo
  "Build a KD-tree over random points and query it.
  Returns {:query-point :nearest-point :distance :nodes-visited}."
  ([] (run-demo 5000 10.0 10))
  ([num-points cube-size num-dimensions]
   (let [points (hypercube-points num-points cube-size num-dimensions)
         tree (build-kdtree points)
         query-point (mapv (fn [_] (rand)) (range num-dimensions))
         [nearest dist visited] (nearest-neighbour-search tree query-point)]
     {:query-point query-point :nearest-point nearest
      :distance dist :nodes-visited visited})))
