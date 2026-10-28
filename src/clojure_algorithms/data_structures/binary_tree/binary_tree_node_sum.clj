(ns clojure-algorithms.data-structures.binary-tree.binary-tree-node-sum
  "Port of data_structures/binary_tree/binary_tree_node_sum.py.
  Sums all node values via depth-first search.")

(defn make-node
  "Create a node with VALUE and optional LEFT/RIGHT children."
  ([value] (make-node value nil nil))
  ([value left right] {:value value :left left :right right}))

(defn node-sum
  "Sum of all values in the tree."
  [node]
  (if (nil? node)
    0
    (+ (:value node) (node-sum (:left node)) (node-sum (:right node)))))
