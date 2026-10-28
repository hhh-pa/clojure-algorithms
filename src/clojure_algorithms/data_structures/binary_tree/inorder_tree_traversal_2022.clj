(ns clojure-algorithms.data-structures.binary-tree.inorder-tree-traversal-2022
  "Port of data_structures/binary_tree/inorder_tree_traversal_2022.py.
  BST insertion with inorder traversal.")

(defn bst-insert
  "Insert NEW-VALUE (duplicates go right); returns the new root."
  [node new-value]
  (cond
    (nil? node) {:data new-value :left nil :right nil}
    (< new-value (:data node))
    (assoc node :left (bst-insert (:left node) new-value))
    :else (assoc node :right (bst-insert (:right node) new-value))))

(defn inorder-values
  "Inorder data sequence."
  [node]
  (when node
    (concat (inorder-values (:left node)) [(:data node)]
            (inorder-values (:right node)))))

(defn make-tree
  "Sample BST built from 15, 10, 25, 6, 14, 20, 60."
  []
  (reduce bst-insert nil [15 10 25 6 14 20 60]))
