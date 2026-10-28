(ns clojure-algorithms.data-structures.binary-tree.binary-tree-maximum-path-sum
  "Port of data_structures/binary_tree/binary_tree_maximum_path_sum.py.
  Maximum path sum between any two nodes.")

(defn make-node
  "Create a node with VAL and optional LEFT/RIGHT children."
  ([val] (make-node val nil nil))
  ([val left right] {:val val :left left :right right}))

(defn construct-tree
  "The sample tree: -10 with 9 and 20(15, 7)."
  []
  (make-node -10 (make-node 9)
             (make-node 20 (make-node 15) (make-node 7))))

(defn- walk [node]
  (if (nil? node)
    [0 -9999999999]
    (let [[left-down left-best] (walk (:left node))
          [right-down right-best] (walk (:right node))
          left-sum (max left-down 0)
          right-sum (max right-down 0)
          down (+ (:val node) (max left-sum right-sum))
          best (max left-best right-best
                    (+ (:val node) left-sum right-sum))]
      [down best])))

(defn max-path-sum
  "Maximum path sum between any two nodes of the tree."
  [root]
  (second (walk root)))
