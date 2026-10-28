(ns clojure-algorithms.machine-learning.frequent-pattern-growth
  "Frequent Pattern Growth (FP-Growth) mining of frequent itemsets.

  Nodes are atoms holding {:name :count :parent :children :node-link}, which
  preserves the shared-identity semantics of the upstream mutable tree (the
  header table aliases live nodes whose counts keep changing). The header
  table itself is an immutable map threaded through purely: update-tree
  returns the updated header.

  Reference: https://athena.ecs.csus.edu/~mei/associationcw/FpGrowth.html")

(defn make-TreeNode
  "Create a tree node atom. Children maps names to node atoms."
  ([name count] (make-TreeNode name count nil {} nil))
  ([name count parent] (make-TreeNode name count parent {} nil))
  ([name count parent children] (make-TreeNode name count parent children nil))
  ([name count parent children node-link]
   (atom {:name name :count count :parent parent
          :children children :node-link node-link})))

(defn- deref-node
  [node]
  (if (instance? clojure.lang.Atom node) @node node))

(defn TreeNode-repr
  "TreeNode('name', count, parent-repr-or-None), recursing into parents."
  [node]
  (let [{:keys [name count parent]} (deref-node node)]
    (str "TreeNode('" name "', " count ", "
         (if (some? parent) (TreeNode-repr parent) "None") ")")))

(defn TreeNode-inc
  "Add num-occur to the node's count. Returns nil."
  [node num-occur]
  (swap! node update :count + num-occur)
  nil)

(defn TreeNode-disp
  "Print the subtree indented. Returns nil."
  ([node] (TreeNode-disp node 1))
  ([node ind]
   (let [{:keys [name count children]} (deref-node node)]
     (println (str (apply str (repeat ind "  ")) " " name "  " count))
     (doseq [child (vals children)]
       (TreeNode-disp child (inc ind))))
   nil))

(defn update-header
  "Append target-node at the end of node's link chain.
  Returns the previous last node (like the upstream code)."
  [node target-node]
  (loop [current node]
    (if (some? (:node-link @current))
      (recur (:node-link @current))
      (do (swap! current assoc :node-link target-node)
          current))))

(defn update-tree
  "Insert items (head-first) under in-tree with the given count, linking new
  nodes into the header table. Returns the updated header map."
  [items in-tree header-table txn-count]
  (let [head (first items)
        header-table
        (if (contains? (:children @in-tree) head)
          (do (TreeNode-inc (get (:children @in-tree) head) txn-count)
              header-table)
          (let [child (make-TreeNode head txn-count in-tree)]
            (swap! in-tree assoc-in [:children head] child)
            (if (nil? (get-in header-table [head 1]))
              (assoc-in header-table [head 1] child)
              (do (update-header (get-in header-table [head 1]) child)
                  header-table))))]
    (if (> (count items) 1)
      (update-tree (vec (rest items))
                   (get (:children @in-tree) head)
                   header-table txn-count)
      header-table)))

(defn create-tree
  "Build the FP-tree for data-set with minimum support min-sup.
  Returns [root-node header-table]; the header maps each item to
  [[support nil] first-node]."
  ([data-set] (create-tree data-set 1))
  ([data-set min-sup]
   (let [counts (reduce (fn [acc trans]
                          (reduce (fn [a item]
                                    (assoc a item [[(inc (get-in a [item 0 0] 0))
                                                    nil] nil]))
                                  acc trans))
                        {} data-set)
         counts (into {} (filter (fn [[_ v]] (>= (get-in v [0 0]) min-sup)))
                      counts)
         freq-items (set (keys counts))]
     (if (empty? freq-items)
       [(make-TreeNode "Null Set" 1 nil) {}]
       (let [header (into {} (map (fn [[k v]] [k v])) counts)
             root (make-TreeNode "Null Set" 1 nil)]
         [root
          (reduce (fn [h tran-set]
                    (let [ordered (->> tran-set
                                       seq
                                       distinct
                                       (filter freq-items)
                                       (map (fn [item] [item (get-in h [item 0 0])]))
                                       (sort-by second >)
                                       (mapv first))]
                      (if (empty? ordered)
                        h
                        (update-tree ordered root h 1))))
                  header data-set)])))))

(defn ascend-tree
  "Names from leaf-node up to (excluding) the root, leaf first.
  Appends onto prefix-path purely and returns the path vector."
  ([leaf-node] (ascend-tree leaf-node []))
  ([leaf-node prefix-path]
   (loop [node (deref-node leaf-node)
          path (vec prefix-path)]
     (if (some? (:parent node))
       (recur (deref-node (:parent node)) (conj path (:name node)))
       path))))

(defn find-prefix-path
  "Conditional pattern base: {prefix-set -> count} for the node chain.
  (base-pat is unused upstream too; it is kept for signature parity.)"
  [_base-pat tree-node]
  (loop [node tree-node
         cond-pats {}]
    (if (nil? node)
      cond-pats
      (let [path (ascend-tree node)]
        (recur (:node-link @node)
               (if (> (count path) 1)
                 (assoc cond-pats (set (rest path)) (:count @node))
                 cond-pats))))))

(defn mine-tree
  "Discover frequent itemsets. Returns (into freq-item-list new-sets).

  NOTE: the upstream `if my_head is not None` is always true (create-tree
  returns a map, never None) and its header-link side effect is unobservable
  through the only output (the itemset list), so this port recurses
  unconditionally without mutating the caller's header."
  [in-tree header-table min-sup pre-fix freq-item-list]
  (let [ordered (mapv first (sort-by #(get-in % [1 0 0]) (seq header-table)))]
    (reduce (fn [found base-pat]
              (let [new-set (conj pre-fix base-pat)
                    found (conj found new-set)
                    cond-bases (find-prefix-path base-pat
                                                 (get-in header-table [base-pat 1]))
                    [cond-tree cond-head] (create-tree (vec (keys cond-bases))
                                                       min-sup)]
                (mine-tree cond-tree cond-head min-sup new-set found)))
            (vec freq-item-list)
            ordered)))
