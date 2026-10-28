(ns clojure-algorithms.data-structures.binary-tree.diameter-of-binary-tree
  "Port of data_structures/binary_tree/diameter_of_binary_tree.py.
  Node depth and (local) diameter in nodes.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn node-depth
  "Depth of the tree in nodes."
  [node]
  (if (nil? node)
    0
    (inc (max (node-depth (:left node)) (node-depth (:right node))))))

(defn node-diameter
  "Diameter through this node: left-depth + right-depth + 1."
  [node]
  (+ (node-depth (:left node)) (node-depth (:right node)) 1))
