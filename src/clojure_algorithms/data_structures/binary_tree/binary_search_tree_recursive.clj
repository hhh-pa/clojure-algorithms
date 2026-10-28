(ns clojure-algorithms.data-structures.binary-tree.binary-search-tree-recursive
  "Port of data_structures/binary_tree/binary_search_tree_recursive.py.
  Recursive BST (duplicate puts raise). Nodes are {:label :left :right}
  maps; a tree is its root (nil when empty).")

(defn bst-put
  "Insert LABEL; returns the new root. Duplicates raise."
  [root label]
  (cond
    (nil? root) {:label label :left nil :right nil}
    (< label (:label root)) (assoc root :left (bst-put (:left root) label))
    (> label (:label root)) (assoc root :right (bst-put (:right root) label))
    :else (throw (ex-info (str "Node with label " label " already exists")
                          {:type :value-error}))))

(defn bst-search
  "Node holding LABEL; raises when absent."
  [root label]
  (cond
    (nil? root) (throw (ex-info (str "Node with label " label " does not exist")
                                {:type :value-error}))
    (< label (:label root)) (bst-search (:left root) label)
    (> label (:label root)) (bst-search (:right root) label)
    :else root))

(defn bst-exists?
  "True when LABEL is in the tree."
  [root label]
  (try (do (bst-search root label) true)
       (catch clojure.lang.ExceptionInfo _ false)))

(defn bst-remove
  "Remove LABEL (successor swap for two children); new root."
  [root label]
  (bst-search root label)
  (letfn [(remove-in [n v]
            (cond
              (< v (:label n)) (assoc n :left (remove-in (:left n) v))
              (> v (:label n)) (assoc n :right (remove-in (:right n) v))
              (nil? (:left n)) (:right n)
              (nil? (:right n)) (:left n)
              :else (let [succ (loop [s (:right n)]
                                 (if (:left s) (recur (:left s)) s))]
                      (assoc n :label (:label succ)
                             :right (remove-in (:right n) (:label succ))))))]
    (remove-in root label)))

(defn bst-max-label
  "Maximum label; raises when empty."
  [root]
  (when (nil? root)
    (throw (ex-info "Binary search tree is empty" {:type :value-error})))
  (loop [n root]
    (if (:right n) (recur (:right n)) (:label n))))

(defn bst-min-label
  "Minimum label; raises when empty."
  [root]
  (when (nil? root)
    (throw (ex-info "Binary search tree is empty" {:type :value-error})))
  (loop [n root]
    (if (:left n) (recur (:left n)) (:label n))))

(defn inorder-labels
  "Inorder label sequence."
  [root]
  (when root
    (concat (inorder-labels (:left root))
            [(:label root)]
            (inorder-labels (:right root)))))

(defn preorder-labels
  "Preorder label sequence."
  [root]
  (when root
    (concat [(:label root)]
            (preorder-labels (:left root))
            (preorder-labels (:right root)))))

(defn bst-empty
  "The empty tree."
  []
  nil)

(defn bst-empty?
  "True when the tree is empty."
  [root]
  (nil? root))
