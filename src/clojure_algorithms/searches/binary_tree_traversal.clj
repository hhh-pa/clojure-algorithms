(ns clojure-algorithms.searches.binary-tree-traversal
  "Binary tree traversals returning value vectors.

  Port of TheAlgorithms/Python searches/binary_tree_traversal.py. Tree
  nodes are immutable TreeNode records; every traversal returns a vector
  of node data (levels nested per level for `level-order-actual`) instead
  of printing. `build-tree` is the pure equivalent of the interactive
  Python builder: it links a level-order value collection into a tree.")

(defrecord TreeNode [data left right])

(defn make-node
  "Build a TreeNode holding `data`, optionally with `left`/`right` children."
  ([data] (->TreeNode data nil nil))
  ([data left right] (->TreeNode data left right)))

(defn build-tree
  "Link level-order `values` into a binary tree; return its root (nil when
  empty). Pure equivalent of the interactive Python builder."
  [values]
  (let [vs (vec values)
        n (count vs)]
    (when (pos? n)
      (loop [i (dec n) nodes (mapv make-node vs)]
        (if (neg? i)
          (nth nodes 0)
          (let [l (inc (* 2 i)) r (+ 2 (* 2 i))]
            (recur (dec i)
                   (assoc nodes i
                          (assoc (nth nodes i)
                                 :left (when (< l n) (nth nodes l))
                                 :right (when (< r n) (nth nodes r)))))))))))

(defn pre-order
  "Root, left, right traversal values."
  [node]
  (when (instance? TreeNode node)
    (vec (concat [(:data node)]
                 (pre-order (:left node))
                 (pre-order (:right node))))))

(defn in-order
  "Left, root, right traversal values."
  [node]
  (when (instance? TreeNode node)
    (vec (concat (in-order (:left node))
                 [(:data node)]
                 (in-order (:right node))))))

(defn post-order
  "Left, right, root traversal values."
  [node]
  (when (instance? TreeNode node)
    (vec (concat (post-order (:left node))
                 (post-order (:right node))
                 [(:data node)]))))

(defn level-order
  "Breadth-first traversal values."
  [node]
  (when (instance? TreeNode node)
    (loop [queue [node] out []]
      (if (empty? queue)
        out
        (let [current (first queue)
              queue (vec (rest queue))
              queue (cond-> queue
                      (:left current) (conj (:left current))
                      (:right current) (conj (:right current)))]
          (recur queue (conj out (:data current))))))))

(defn level-order-actual
  "Breadth-first values grouped one vector per tree level."
  [node]
  (when (instance? TreeNode node)
    (loop [queue [node] out []]
      (if (empty? queue)
        out
        (let [level (mapv :data queue)
              children (vec (mapcat #(filter some? [(:left %) (:right %)])
                                    queue))]
          (recur children (conj out level)))))))

(defn pre-order-iter
  "Iterative root, left, right traversal values."
  [node]
  (when (instance? TreeNode node)
    (loop [n node stack [] out []]
      (cond n (recur (:left n) (conj stack n) (conj out (:data n)))
            (empty? stack) out
            :else (recur (:right (peek stack)) (pop stack) out)))))

(defn in-order-iter
  "Iterative left, root, right traversal values."
  [node]
  (when (instance? TreeNode node)
    (loop [n node stack [] out []]
      (cond n (recur (:left n) (conj stack n) out)
            (empty? stack) out
            :else (let [top (peek stack)]
                    (recur (:right top) (pop stack)
                           (conj out (:data top))))))))

(defn post-order-iter
  "Iterative left, right, root traversal values."
  [node]
  (when (instance? TreeNode node)
    (let [stack2 (loop [stack1 [node] stack2 []]
                   (if (empty? stack1)
                     stack2
                     (let [n (peek stack1)
                           stack1 (cond-> (pop stack1)
                                    (:left n) (conj (:left n))
                                    (:right n) (conj (:right n)))]
                       (recur stack1 (conj stack2 n)))))]
      (mapv :data (rseq (vec stack2))))))

(defn- code-point-count
  "Number of Unicode code points in string `s`."
  [s]
  (.codePointCount ^String s 0 (.length ^String s)))

(defn prompt
  "Pad `s` with `ch` to `width` (defaults 50 and \"*\").

  With an empty string returns a newline plus width copies of `ch`,
  mirroring the Python helper."
  ([s] (prompt s 50 "*"))
  ([s width] (prompt s width "*"))
  ([s width ch]
   (let [ch (str ch)]
     (if (empty? s)
       (str "\n" (apply str (repeat width ch)))
       (let [total (- width (code-point-count (str s)) 2)
             left (Math/floorDiv total 2)
             extra (Math/floorMod total 2)
             left (max left 0)]
         (str (apply str (repeat left ch)) " " s " "
              (apply str (repeat (+ left extra) ch))))))))
