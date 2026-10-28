(ns clojure-algorithms.data-structures.kd-tree.kd-node
  "Port of data_structures/kd_tree/kd_node.py.
  KD-tree nodes are {:point :left :right} maps.")

(defn make-kd-node
  "Create a KD node holding POINT with optional children."
  ([point] (make-kd-node point nil nil))
  ([point left right] {:point (vec point) :left left :right right}))
