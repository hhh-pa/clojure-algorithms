(ns clojure-algorithms.data-structures.binary-tree.merge-two-binary-trees
  "Port of data_structures/binary_tree/merge_two_binary_trees.py.
  Overlays two trees summing overlaps (pure: returns a new tree).")

(defn make-node
  "Create a node with VALUE and optional LEFT/RIGHT children."
  ([value] (make-node value nil nil))
  ([value left right] {:value value :left left :right right}))

(defn merge-trees
  "New tree overlaying T1 and T2, summing overlapping values."
  [t1 t2]
  (cond
    (nil? t1) t2
    (nil? t2) t1
    :else (make-node (+ (:value t1) (:value t2))
                     (merge-trees (:left t1) (:left t2))
                     (merge-trees (:right t1) (:right t2)))))

(defn preorder-values
  "Preorder value sequence."
  [root]
  (when root
    (concat [(:value root)]
            (preorder-values (:left root))
            (preorder-values (:right root)))))
