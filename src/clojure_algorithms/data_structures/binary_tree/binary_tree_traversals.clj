(ns clojure-algorithms.data-structures.binary-tree.binary-tree-traversals
  "Port of data_structures/binary_tree/binary_tree_traversals.py.
  Standard binary-tree traversals.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn make-tree
  "The sample tree: 1 with 2(4, 5) and 3."
  []
  (make-node 1 (make-node 2 (make-node 4) (make-node 5)) (make-node 3)))

(defn preorder
  "Preorder data sequence."
  [root]
  (when root
    (concat [(:data root)] (preorder (:left root)) (preorder (:right root)))))

(defn postorder
  "Postorder data sequence."
  [root]
  (when root
    (concat (postorder (:left root)) (postorder (:right root)) [(:data root)])))

(defn inorder
  "Inorder data sequence."
  [root]
  (when root
    (concat (inorder (:left root)) [(:data root)] (inorder (:right root)))))

(defn reverse-inorder
  "Reverse-inorder data sequence."
  [root]
  (when root
    (concat (reverse-inorder (:right root)) [(:data root)]
            (reverse-inorder (:left root)))))

(defn tree-height
  "Height of the tree (0 for nil)."
  [root]
  (if (nil? root)
    0
    (inc (max (tree-height (:left root)) (tree-height (:right root))))))

(defn level-order
  "Breadth-first data sequence."
  [root]
  (when root
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY root) out []]
      (if (empty? queue)
        out
        (let [node (peek queue)
              queue (pop queue)]
          (recur (cond-> queue
                   (:left node) (conj (:left node))
                   (:right node) (conj (:right node)))
                 (conj out (:data node))))))))

(defn nodes-left-to-right
  "Data values at 1-based LEVEL, left to right."
  [root level]
  (cond
    (nil? root) []
    (= level 1) [(:data root)]
    :else (vec (concat (nodes-left-to-right (:left root) (dec level))
                       (nodes-left-to-right (:right root) (dec level))))))

(defn nodes-right-to-left
  "Data values at 1-based LEVEL, right to left."
  [root level]
  (cond
    (nil? root) []
    (= level 1) [(:data root)]
    :else (vec (concat (nodes-right-to-left (:right root) (dec level))
                       (nodes-right-to-left (:left root) (dec level))))))

(defn zigzag
  "Zigzag (alternating direction) level-order data sequence."
  [root]
  (when root
    (vec (mapcat (fn [h]
                   (if (odd? h)
                     (nodes-left-to-right root h)
                     (nodes-right-to-left root h)))
                 (range 1 (inc (tree-height root)))))))
