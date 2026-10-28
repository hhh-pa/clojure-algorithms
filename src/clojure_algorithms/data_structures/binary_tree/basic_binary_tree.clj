(ns clojure-algorithms.data-structures.binary-tree.basic-binary-tree
  "Port of data_structures/binary_tree/basic_binary_tree.py.
  Nodes are {:data :left :right} maps; a tree is its root.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn inorder-values
  "Inorder data sequence of the tree."
  [node]
  (when node
    (concat (inorder-values (:left node))
            [(:data node)]
            (inorder-values (:right node)))))

(defn tree-count
  "Number of nodes in the tree."
  [node]
  (count (inorder-values node)))

(defn small-tree
  "The 3-node sample tree: 2 with children 1 and 3."
  []
  (make-node 2 (make-node 1) (make-node 3)))

(defn medium-tree
  "The 7-node sample tree."
  []
  (make-node 4
             (make-node 2 (make-node 1) (make-node 3))
             (make-node 5 nil (make-node 6 nil (make-node 7)))))

(defn tree-depth
  "Depth (height) of the tree."
  [node]
  (if (nil? node)
    0
    (inc (max (tree-depth (:left node)) (tree-depth (:right node))))))

(defn full-tree?
  "True when every node has 0 or 2 children."
  [node]
  (cond
    (nil? node) true
    (and (nil? (:left node)) (nil? (:right node))) true
    (and (:left node) (:right node))
    (and (full-tree? (:left node)) (full-tree? (:right node)))
    :else false))
