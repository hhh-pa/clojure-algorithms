(ns clojure-algorithms.data-structures.binary-tree.floor-and-ceiling
  "Port of data_structures/binary_tree/floor_and_ceiling.py.
  Floor and ceiling of a key in a BST.")

(defn make-node
  "Create a node with KEY and optional LEFT/RIGHT children."
  ([key] (make-node key nil nil))
  ([key left right] {:key key :left left :right right}))

(defn inorder-keys
  "Inorder key sequence."
  [node]
  (when node
    (concat (inorder-keys (:left node)) [(:key node)]
            (inorder-keys (:right node)))))

(defn floor-ceiling
  "Pair [floor ceiling] of KEY in the BST (nil when missing)."
  [root key]
  (loop [node root floor-val nil ceiling-val nil]
    (if (nil? node)
      [floor-val ceiling-val]
      (cond
        (= (:key node) key) [(:key node) (:key node)]
        (< key (:key node)) (recur (:left node) floor-val (:key node))
        :else (recur (:right node) (:key node) ceiling-val)))))
