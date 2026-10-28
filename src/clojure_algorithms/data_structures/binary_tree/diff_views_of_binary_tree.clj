(ns clojure-algorithms.data-structures.binary-tree.diff-views-of-binary-tree
  "Port of data_structures/binary_tree/diff_views_of_binary_tree.py.
  Right, left, top and bottom views of a binary tree.")

(defn make-node
  "Create a node with VAL and optional LEFT/RIGHT children."
  ([val] (make-node val nil nil))
  ([val left right] {:val val :left left :right right}))

(defn make-tree
  "The sample tree: 3 with 9 and 20(15, 7)."
  []
  (make-node 3 (make-node 9)
             (make-node 20 (make-node 15) (make-node 7))))

(defn right-side-view
  "Right side view values."
  [root]
  (letfn [(dfs [node depth view]
            (if (nil? node)
              view
              (let [view (if (= depth (count view))
                           (conj view (:val node)) view)]
                (dfs (:left node) (inc depth)
                     (dfs (:right node) (inc depth) view)))))]
    (if (nil? root) [] (dfs root 0 []))))

(defn left-side-view
  "Left side view values."
  [root]
  (letfn [(dfs [node depth view]
            (if (nil? node)
              view
              (let [view (if (= depth (count view))
                           (conj view (:val node)) view)]
                (dfs (:right node) (inc depth)
                     (dfs (:left node) (inc depth) view)))))]
    (if (nil? root) [] (dfs root 0 []))))

(defn- hd-lookup [root]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [root 0]) lookup {}]
    (if (empty? queue)
      lookup
      (let [[[node hd] & _] (list (peek queue))
            queue (pop queue)
            lookup (update lookup hd (fnil conj []) (:val node))]
        (recur (cond-> queue
                 (:left node) (conj [(:left node) (dec hd)])
                 (:right node) (conj [(:right node) (inc hd)]))
               lookup)))))

(defn top-side-view
  "Top view values, left to right."
  [root]
  (if (nil? root)
    []
    (mapv (fn [[_ vs]] (first vs)) (sort-by key (hd-lookup root)))))

(defn bottom-side-view
  "Bottom view values, left to right."
  [root]
  (if (nil? root)
    []
    (mapv (fn [[_ vs]] (peek vs)) (sort-by key (hd-lookup root)))))
