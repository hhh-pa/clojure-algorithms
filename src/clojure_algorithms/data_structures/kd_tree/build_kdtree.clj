(ns clojure-algorithms.data-structures.kd-tree.build-kdtree
  "Port of data_structures/kd_tree/build_kdtree.py."
  (:require [clojure-algorithms.data-structures.kd-tree.kd-node
             :refer [make-kd-node]]))

(defn build-kdtree
  "Build a KD-tree over POINTS (median splits); nil when empty."
  ([points] (build-kdtree points 0))
  ([points depth]
   (when (seq points)
     (let [k (count (first points))
           axis (mod depth k)
           sorted-pts (vec (sort-by #(nth % axis) points))
           median-idx (quot (count sorted-pts) 2)]
       (make-kd-node (nth sorted-pts median-idx)
                     (build-kdtree (subvec sorted-pts 0 median-idx)
                                   (inc depth))
                     (build-kdtree (subvec sorted-pts (inc median-idx))
                                   (inc depth)))))))
