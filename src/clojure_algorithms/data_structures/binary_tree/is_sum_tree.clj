(ns clojure-algorithms.data-structures.binary-tree.is-sum-tree
  "Port of data_structures/binary_tree/is_sum_tree.py.
  A sum tree has every node's data equal to the total of its subtrees.")

(defn make-node
  "Create a node with DATA and optional LEFT/RIGHT children."
  ([data] (make-node data nil nil))
  ([data left right] {:data data :left left :right right}))

(defn inorder-values
  "Inorder data sequence."
  [node]
  (when node
    (concat (inorder-values (:left node)) [(:data node)]
            (inorder-values (:right node)))))

(defn tree-count
  "Number of nodes."
  [node]
  (count (inorder-values node)))

(defn sum-node?
  "True when NODE is a sum node (leaves qualify)."
  [node]
  (if (and (nil? (:left node)) (nil? (:right node)))
    true
    (let [left-sum (reduce + 0 (inorder-values (:left node)))
          right-sum (reduce + 0 (inorder-values (:right node)))]
      (and (= (:data node) (+ left-sum right-sum))
           (if (:left node) (sum-node? (:left node)) true)
           (if (:right node) (sum-node? (:right node)) true)))))

(defn sum-tree?
  "True when the whole tree is a sum tree."
  [root]
  (sum-node? root))

(defn build-a-tree
  "A non-sum sample tree."
  []
  (make-node 11 (make-node 2 (make-node 1) (make-node 7))
             (make-node 29 (make-node 15)
                        (make-node 40 (make-node 35) nil))))

(defn build-a-sum-tree
  "A sum-tree sample."
  []
  (make-node 26 (make-node 10 (make-node 4) (make-node 6))
             (make-node 3 nil (make-node 3))))
