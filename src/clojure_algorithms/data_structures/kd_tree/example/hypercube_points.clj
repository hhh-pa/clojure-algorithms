(ns clojure-algorithms.data-structures.kd-tree.example.hypercube-points
  "Port of data_structures/kd_tree/example/hypercube_points.py.
  Uniform random points in an n-dimensional hypercube
  (returns nested vectors instead of a numpy array).")

(defn hypercube-points
  "NUM-POINTS random points uniform in [0, HYPERCUBE-SIZE)^NUM-DIMENSIONS."
  [num-points hypercube-size num-dimensions]
  (mapv (fn [_] (mapv (fn [_] (* hypercube-size (rand))) (range num-dimensions)))
        (range num-points)))
