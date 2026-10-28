(ns clojure-algorithms.sorts.tree-sort
  "Tree sort: insert into a binary search tree, then in-order traversal.
  Ported from TheAlgorithms/Python.")

(defn- insert-node
  "Inserts `x` into BST node (nil or {:val :left :right}).
  Equal values go right so duplicates are kept."
  [node x]
  (if (nil? node)
    {:val x :left nil :right nil}
    (if (neg? (compare x (:val node)))
      (assoc node :left (insert-node (:left node) x))
      (assoc node :right (insert-node (:right node) x)))))

(defn- inorder
  "Lazy in-order traversal of BST `node`."
  [node]
  (when node
    (concat (inorder (:left node)) [(:val node)] (inorder (:right node)))))

(defn tree-sort
  "Sorts `coll` with tree sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (vec (inorder (reduce insert-node nil v))))))
