(ns clojure-algorithms.data-structures.binary-tree.symmetric-tree
  "Port of data_structures/binary_tree/symmetric_tree.py.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn make-symmetric-tree
  "Symmetric sample tree."
  []
  (make-node 1 (make-node 2 (make-node 3) (make-node 4))
             (make-node 2 (make-node 4) (make-node 3))))

(defn make-asymmetric-tree
  "Asymmetric sample tree."
  []
  (make-node 1 (make-node 2 (make-node 3) (make-node 4))
             (make-node 2 (make-node 3) (make-node 4))))

(defn is-mirror?
  "True when LEFT and RIGHT are mirrors of each other."
  [left right]
  (cond
    (and (nil? left) (nil? right)) true
    (or (nil? left) (nil? right)) false
    (= (:data left) (:data right))
    (and (is-mirror? (:left left) (:right right))
         (is-mirror? (:right left) (:left right)))
    :else false))

(defn symmetric-tree?
  "True when TREE is symmetric (nil counts as symmetric)."
  [tree]
  (if (nil? tree) true (is-mirror? (:left tree) (:right tree))))
