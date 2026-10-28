(ns clojure-algorithms.data-structures.binary-tree.binary-tree-path-sum
  "Port of data_structures/binary_tree/binary_tree_path_sum.py.
  Counts downward paths summing to a target (paths may start anywhere).")

(defn make-node
  "Create a node with VALUE and optional LEFT/RIGHT children."
  ([value] (make-node value nil nil))
  ([value left right] {:value value :left left :right right}))

(defn- count-from [node target running]
  (if (nil? node)
    0
    (+ (if (= running target) 1 0)
       (if (:left node)
         (count-from (:left node) target (+ running (:value (:left node))))
         0)
       (if (:right node)
         (count-from (:right node) target (+ running (:value (:right node))))
         0))))

(defn path-sum-count
  "Number of downward paths in the tree summing to TARGET."
  [node target]
  (if (nil? node)
    0
    (+ (count-from node target (:value node))
       (path-sum-count (:left node) target)
       (path-sum-count (:right node) target))))
