(ns clojure-algorithms.data-structures.binary-tree.is-sorted
  "Port of data_structures/binary_tree/is_sorted.py.
  Checks the BST ordering property locally at each node.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn inorder-values
  "Inorder data sequence."
  [node]
  (when node
    (concat (inorder-values (:left node)) [(:data node)]
            (inorder-values (:right node)))))

(defn sorted-tree?
  "True when each node's data dominates its left and is dominated
  by its right subtree, recursively."
  [node]
  (and (or (nil? (:left node))
           (and (>= (:data node) (:data (:left node)))
                (sorted-tree? (:left node))))
       (or (nil? (:right node))
           (and (<= (:data node) (:data (:right node)))
                (sorted-tree? (:right node))))))
