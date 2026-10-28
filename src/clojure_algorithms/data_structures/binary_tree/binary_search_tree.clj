(ns clojure-algorithms.data-structures.binary-tree.binary-search-tree
  "Port of data_structures/binary_tree/binary_search_tree.py.
  Iterative BST. Nodes are {:value :left :right} maps; a tree is its
  root (nil when empty). Duplicates go right.")

(defn bst-insert-value
  "Insert VALUE into the tree; returns the new root."
  [root value]
  (if (nil? root)
    {:value value :left nil :right nil}
    (loop [node root stack []]
      (if (< value (:value node))
        (if (nil? (:left node))
          (reduce (fn [child [parent side]]
                    (assoc parent side child))
                  {:value value :left nil :right nil}
                  (reverse (conj stack [node :left])))
          (recur (:left node) (conj stack [node :left])))
        (if (nil? (:right node))
          (reduce (fn [child [parent side]]
                    (assoc parent side child))
                  {:value value :left nil :right nil}
                  (reverse (conj stack [node :right])))
          (recur (:right node) (conj stack [node :right])))))))

(defn bst-insert
  "Insert VALUES left to right; returns the new root."
  [root & values]
  (reduce bst-insert-value root values))

(defn bst-search
  "Node holding VALUE, or nil. Throws on an empty tree."
  [root value]
  (when (nil? root)
    (throw (ex-info "Warning: Tree is empty! please use another."
                    {:type :index-error})))
  (loop [node root]
    (cond
      (nil? node) nil
      (= value (:value node)) node
      (< value (:value node)) (recur (:left node))
      :else (recur (:right node)))))

(defn bst-contains?
  "True when VALUE is in the tree."
  [root value]
  (if (nil? root)
    false
    (some? (bst-search root value))))

(defn bst-max
  "Node with the maximum value at or under ROOT."
  [root]
  (when root
    (loop [node root]
      (if (:right node) (recur (:right node)) node))))

(defn bst-min
  "Node with the minimum value at or under ROOT."
  [root]
  (when root
    (loop [node root]
      (if (:left node) (recur (:left node)) node))))

(defn bst-remove
  "Remove VALUE (predecessor swap for two children); new root."
  [root value]
  (let [node (if (nil? root) nil (bst-search root value))]
    (when (nil? node)
      (throw (ex-info (str "Value " value " not found") {:type :value-error})))
    (letfn [(remove-in [n v]
              (cond
                (< v (:value n)) (assoc n :left (remove-in (:left n) v))
                (> v (:value n)) (assoc n :right (remove-in (:right n) v))
                (nil? (:left n)) (:right n)
                (nil? (:right n)) (:left n)
                :else (let [pred (:value (bst-max (:left n)))]
                        (assoc n :value pred
                               :left (remove-in (:left n) pred)))))]
      (remove-in root value))))

(defn inorder-values
  "Inorder value sequence."
  [root]
  (when root
    (concat (inorder-values (:left root))
            [(:value root)]
            (inorder-values (:right root)))))

(defn preorder-values
  "Preorder value sequence."
  [root]
  (when root
    (concat [(:value root)]
            (preorder-values (:left root))
            (preorder-values (:right root)))))

(defn postorder-values
  "Postorder value sequence."
  [root]
  (when root
    (concat (postorder-values (:left root))
            (postorder-values (:right root))
            [(:value root)])))

(defn traversal-tree
  "Preorder values by default; pass :inorder or :postorder to vary."
  ([root] (preorder-values root))
  ([root order]
   (case order
     :inorder (inorder-values root)
     :postorder (postorder-values root)
     (preorder-values root))))

(defn find-kth-smallest
  "Kth smallest value (1-based)."
  [root k]
  (nth (inorder-values root) (dec k)))

(defn bst-empty?
  "True when the tree is empty."
  [root]
  (nil? root))
