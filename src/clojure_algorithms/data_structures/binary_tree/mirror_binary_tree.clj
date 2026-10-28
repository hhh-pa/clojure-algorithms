(ns clojure-algorithms.data-structures.binary-tree.mirror-binary-tree
  "Port of data_structures/binary_tree/mirror_binary_tree.py.
  Mirrors a binary tree (pure: returns a new tree).")

(defn make-node
  "Create a node with VALUE and optional LEFT/RIGHT children."
  ([value] (make-node value nil nil))
  ([value left right] {:value value :left left :right right}))

(defn inorder-values
  "Inorder value sequence."
  [node]
  (when node
    (concat (inorder-values (:left node)) [(:value node)]
            (inorder-values (:right node)))))

(defn mirror-tree
  "New tree with left/right children swapped at every node."
  [node]
  (when node
    (make-node (:value node)
               (mirror-tree (:right node))
               (mirror-tree (:left node)))))

(defn make-tree-seven
  "Complete 7-node sample tree."
  []
  (make-node 1 (make-node 2 (make-node 4) (make-node 5))
             (make-node 3 (make-node 6) (make-node 7))))

(defn make-tree-nine
  "9-node sample tree."
  []
  (make-node 1 (make-node 2 (make-node 4 (make-node 7) (make-node 8))
                          (make-node 5 nil (make-node 9)))
             (make-node 3 nil (make-node 6))))
