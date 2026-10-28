(ns clojure-algorithms.data-structures.binary-tree.flatten-binarytree-to-linkedlist
  "Port of data_structures/binary_tree/flatten_binarytree_to_linkedlist.py.
  Flattens a binary tree to a right-skewed chain in preorder
  (pure: returns a new tree).")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn build-tree
  "The sample tree: 1 with 2(3, 4) and 5(nil, 6)."
  []
  (make-node 1 (make-node 2 (make-node 3) (make-node 4))
             (make-node 5 nil (make-node 6))))

(defn- preorder-values [node]
  (when node
    (concat [(:data node)]
            (preorder-values (:left node))
            (preorder-values (:right node)))))

(defn flatten-tree
  "Right-skewed chain of the tree in preorder."
  [root]
  (reduce (fn [right data] (make-node data nil right))
          nil (reverse (preorder-values root))))

(defn chain-values
  "Data values following right links from ROOT."
  [root]
  (loop [node root out []]
    (if (nil? node) out
        (recur (:right node) (conj out (:data node))))))
