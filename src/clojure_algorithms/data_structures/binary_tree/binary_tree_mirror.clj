(ns clojure-algorithms.data-structures.binary-tree.binary-tree-mirror
  "Port of data_structures/binary_tree/binary_tree_mirror.py.
  Mirrors a dict-shaped tree {node -> [left right]} (pure: returns a map).")

(defn- mirror-from [tree root]
  (if (or (nil? root) (false? root) (= root 0) (not (contains? tree root)))
    tree
    (let [[left right] (take 2 (get tree root))
          tree (assoc tree root [right left])]
      (-> tree
          (mirror-from left)
          (mirror-from right)))))

(defn binary-tree-mirror
  "New map with every [left right] pair below ROOT swapped."
  ([tree] (binary-tree-mirror tree 1))
  ([tree root]
   (when (empty? tree)
     (throw (ex-info "binary tree cannot be empty" {:type :value-error})))
   (when-not (contains? tree root)
     (throw (ex-info (str "root " root " is not present in the binary_tree")
                     {:type :value-error})))
   (mirror-from tree root)))
