(ns clojure-algorithms.data-structures.binary-tree.avl-tree
  "Port of data_structures/binary_tree/avl_tree.py.
  Functional AVL tree. Nodes are {:data :left :right :height} maps.
  (Rotation/insertion print-outs from the Python version are omitted.)")

(defn make-node
  "Create an AVL node holding DATA."
  [data]
  {:data data :left nil :right nil :height 1})

(defn node-height
  "Height of NODE (0 for nil)."
  [node]
  (if (nil? node) 0 (:height node)))

(defn- fix-height [node]
  (assoc node :height (inc (max (node-height (:left node))
                                (node-height (:right node))))))

(defn right-rotation
  "Right rotation around NODE."
  [node]
  (let [ret (:left node)
        node (assoc node :left (:right ret))
        node (fix-height node)
        ret (assoc ret :right node)]
    (fix-height ret)))

(defn left-rotation
  "Left rotation around NODE."
  [node]
  (let [ret (:right node)
        node (assoc node :right (:left ret))
        node (fix-height node)
        ret (assoc ret :left node)]
    (fix-height ret)))

(defn lr-rotation
  "Left-right rotation around NODE."
  [node]
  (right-rotation (assoc node :left (left-rotation (:left node)))))

(defn rl-rotation
  "Right-left rotation around NODE."
  [node]
  (left-rotation (assoc node :right (right-rotation (:right node)))))

(defn insert-node
  "Insert DATA into the AVL tree rooted at NODE. Duplicates go right."
  [node data]
  (if (nil? node)
    (make-node data)
    (if (< data (:data node))
      (let [node (assoc node :left (insert-node (:left node) data))]
        (if (= (- (node-height (:left node)) (node-height (:right node))) 2)
          (let [left-child (:left node)
                node (if (< data (:data left-child))
                       (right-rotation node)
                       (lr-rotation node))]
            (fix-height node))
          (fix-height node)))
      (let [node (assoc node :right (insert-node (:right node) data))]
        (if (= (- (node-height (:right node)) (node-height (:left node))) 2)
          (let [right-child (:right node)
                node (if (< data (:data right-child))
                       (rl-rotation node)
                       (left-rotation node))]
            (fix-height node))
          (fix-height node))))))

(defn get-left-most
  "Smallest data in the tree rooted at NODE."
  [node]
  (loop [n node]
    (if (:left n) (recur (:left n)) (:data n))))

(defn get-right-most
  "Largest data in the tree rooted at NODE."
  [node]
  (loop [n node]
    (if (:right n) (recur (:right n)) (:data n))))

(defn del-node
  "Delete DATA from the AVL tree rooted at ROOT."
  [root data]
  (let [left-child (:left root)
        right-child (:right root)]
    (cond
      (= (:data root) data)
      (cond
        (and left-child right-child)
        (let [temp (get-left-most right-child)]
          (assoc root :data temp
                 :right (del-node right-child temp)))
        left-child left-child
        right-child right-child
        :else nil)
      (> (:data root) data)
      (if (nil? left-child)
        root
        (assoc root :left (del-node left-child data)))
      (nil? right-child) root
      :else (assoc root :right (del-node right-child data)))))

(defn- rebalance-after-delete [root]
  (if (nil? root)
    nil
    (let [left-child (:left root)
          right-child (:right root)
          balance (- (node-height right-child) (node-height left-child))
          root (cond
                 (= balance 2)
                 (if (> (node-height (:right right-child))
                        (node-height (:left right-child)))
                   (left-rotation root)
                   (rl-rotation root))
                 (= balance -2)
                 (if (> (node-height (:left left-child))
                        (node-height (:right left-child)))
                   (right-rotation root)
                   (lr-rotation root))
                 :else root)]
      (fix-height root))))

(defn avl-delete
  "Delete DATA from ROOT, rebalancing; nil-safe."
  [root data]
  (if (nil? root)
    nil
    (rebalance-after-delete (del-node root data))))

(defn inorder-values
  "Inorder data sequence of the tree."
  [node]
  (when node
    (concat (inorder-values (:left node))
            [(:data node)]
            (inorder-values (:right node)))))
