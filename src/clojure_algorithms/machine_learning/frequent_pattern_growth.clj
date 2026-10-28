(ns clojure-algorithms.machine-learning.frequent-pattern-growth
  "The Frequent Pattern Growth algorithm (FP-Growth) is a widely used data mining\ntechnique for discovering frequent itemsets in large transaction databases.\n\nIt overcomes some of the limitations of traditional methods such as Apriori by\nefficiently constructing the FP-Tree\n\nWIKI: https://athena.ecs.csus.edu/~mei/associationcw/FpGrowth.html\n\nExamples: https://www.javatpoint.com/fp-growth-algorithm-in-data-mining"
  (:require [clojure.string :as str]))

(declare create-tree update-tree update-header ascend-tree find-prefix-path mine-tree make-TreeNode TreeNode-repr TreeNode-inc TreeNode-disp)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-dissoc-one py-dissoc-in py-slice py-add py-repeat-vec py-mul py-sorted py-list py-set py-dict py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-dissoc-one [c k]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) [])
        (map? c) (if (contains? c k) (dissoc c k)
                     (throw (ex-info (str "key: " k) {:type :KeyError})))
        (string? c) (throw (ex-info "does not support item deletion" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "deletion index out of range" {:type :IndexError})))
                (vec (concat (subvec vv 0 ii) (subvec vv (inc ii)))))))

(defn- py-dissoc-in [c ks]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-dissoc-in (py-get c k) more))
        (let [r (py-dunder :delitem c k)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (dissoc c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-dissoc-in (py-get c k) more))
        (py-dissoc-one c k)))))

(defn- py-slice [c lo hi step]
  (when (map? c) (throw (ex-info "unhashable slice" {:type :TypeError})))
  (let [s (vec (seq c)) n (count s)
        step (if (nil? step) 1 step)]
    (when (zero? step) (throw (ex-info "slice step cannot be zero" {:type :ValueError})))
    (let [idxs (if (pos? step)
                 (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                        (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                        step)
                 (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                        (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                        step))
          res (mapv s idxs)]
      (if (string? c) (apply str res) res))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-dict [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (sequential? x) (into {} (map (fn [p] [(nth p 0) (nth p 1)]) x))
        :else (throw (ex-info "cannot convert to dict" {:type :TypeError}))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defrecord TreeNode [name count-v parent children node-link])

(defn make-TreeNode "Constructor for TreeNode. A node in a Frequent Pattern tree.\n\nArgs:\n    name: The name of this node.\n    num_occur: The number of occurrences of the node.\n    parent_node: The parent node.\n\nExample:\n>>> parent = TreeNode(\"Parent\", 1, None)\n>>> child = TreeNode(\"Child\", 2, parent)\n>>> child.name\n'Child'\n>>> child.count\n2"
  ([name count-v] (make-TreeNode name count-v nil (py-dict {}) nil))
  ([name count-v parent] (make-TreeNode name count-v parent (py-dict {}) nil))
  ([name count-v parent children] (make-TreeNode name count-v parent children nil))
  ([name count-v parent children node-link] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:name] name))) (swap! self (fn [__c] (py-assoc-in __c [:count] count-v))) (swap! self (fn [__c] (py-assoc-in __c [:parent] parent))) (swap! self (fn [__c] (py-assoc-in __c [:children] children))) (swap! self (fn [__c] (py-assoc-in __c [:node-link] node-link))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->TreeNode @self))))

(defn TreeNode-repr
  [self] (str "TreeNode(" (py-str (pr-str (:name @self))) ", " (py-str (pr-str (:count @self))) ", " (py-str (pr-str (:parent @self))) ")"))

(defn TreeNode-inc
  [self num-occur] (swap! self (fn [__c] (py-assoc-in __c [:count] (py-add (py-get-in __c [:count]) num-occur)))) nil)

(defn TreeNode-disp
  ([self] (TreeNode-disp self 1))
  ([self ind] (let [child (atom nil)] (println (clojure.string/join " " [(py-str (str (py-str (py-mul "  " ind)) " " (py-str (:name @self)) "  " (py-str (:count @self))))])) (doseq [__i1 (py-iter (vec (vals (:children @self))))] (reset! child __i1) (TreeNode-disp child (py-add ind 1))) nil)))

(defn create-tree "Create Frequent Pattern tree\n\nArgs:\n    data_set: A list of transactions, where each transaction is a list of items.\n    min_sup: The minimum support threshold.\n    Items with support less than this will be pruned. Default is 1.\n\nReturns:\n    The root of the FP-Tree.\n    header_table: The header table dictionary with item information.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> len(header_table)\n4\n>>> header_table[\"A\"]\n[[4, None], TreeNode('A', 4, TreeNode('Null Set', 1, None))]\n>>> header_table[\"E\"][1]  # doctest: +NORMALIZE_WHITESPACE\nTreeNode('E', 1, TreeNode('B', 3, TreeNode('A', 4, TreeNode('Null Set', 1, None))))\n>>> sorted(header_table)\n['A', 'B', 'C', 'E']\n>>> fp_tree.name\n'Null Set'\n>>> sorted(fp_tree.children)\n['A', 'B']\n>>> fp_tree.children['A'].name\n'A'\n>>> sorted(fp_tree.children['A'].children)\n['B', 'C']"
  ([data-set] (create-tree data-set 1))
  ([data-set min-sup] (let [fp-tree (atom nil) freq-item-set (atom nil) header-table (atom nil) item (atom nil) k (atom nil) key (atom nil) local-d (atom nil) ordered-items (atom nil) sorted-items (atom nil) tran-set (atom nil) trans (atom nil) value (atom nil)] (try (reset! header-table {}) (doseq [__i1 (py-iter data-set)] (reset! trans __i1) (doseq [__i2 (py-iter @trans)] (reset! item __i2) (swap! header-table (fn [__c] (py-assoc-in __c [@item] (get @header-table @item [0 nil])))) (swap! header-table (fn [__c] (py-assoc-in __c [@item 0] (py-add (py-get-in __c [@item 0]) 1)))))) (doseq [__i3 (py-iter (py-list @header-table))] (reset! k __i3) (when (py-truthy (py-lt (py-get (py-get @header-table @k) 0) min-sup)) (swap! header-table (fn [__c] (py-dissoc-in __c [@k]))))) (when (py-truthy (not (py-truthy (do (reset! freq-item-set (py-set @header-table)) @freq-item-set)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(make-TreeNode "Null Set" 1 nil) {}]}))) (doseq [[__i4 __i5] (py-iter (vec @header-table))] (reset! key __i4) (reset! value __i5) (swap! header-table (fn [__c] (py-assoc-in __c [@key] [@value nil])))) (reset! fp-tree (make-TreeNode "Null Set" 1 nil)) (doseq [__i6 (py-iter data-set)] (reset! tran-set __i6) (reset! local-d (into {} (for [item (py-iter @tran-set) :when (py-truthy (py-in item @freq-item-set))] (list [item (py-get (py-get @header-table item) 0)])))) (when (py-truthy @local-d) (reset! sorted-items (if (py-truthy true) (vec (reverse (sort-by (fn [item-info] (py-get item-info 1)) (vec @local-d)))) (vec (sort-by (fn [item-info] (py-get item-info 1)) (vec @local-d))))) (reset! ordered-items (vec (for [item (py-iter @sorted-items)] (py-get item 0)))) (update-tree @ordered-items @fp-tree @header-table 1))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@fp-tree @header-table]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn update-tree "Update the FP-Tree with a transaction.\n\nArgs:\n    items: List of items in the transaction.\n    in_tree: The current node in the FP-Tree.\n    header_table: The header table dictionary with item information.\n    count: The count of the transaction.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> transaction = ['A', 'B', 'E']\n>>> update_tree(transaction, fp_tree, header_table, 1)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> fp_tree.children['A'].children['B'].children['E'].children\n{}\n>>> fp_tree.children['A'].children['B'].children['E'].count\n2\n>>> header_table['E'][1].name\n'E'"
  [items in-tree header-table count-v] (let [header-table (atom header-table)] (if (py-truthy (py-in (py-get items 0) (:children in-tree))) (do (TreeNode-inc (atom (py-get (:children in-tree) (py-get items 0))) count-v)) (do nil (if (py-truthy (nil? (py-get (py-get @header-table (py-get items 0)) 1))) (do (swap! header-table (fn [__c] (py-assoc-in __c [(py-get items 0) 1] (py-get (:children in-tree) (py-get items 0)))))) (do (update-header (py-get (py-get @header-table (py-get items 0)) 1) (py-get (:children in-tree) (py-get items 0))))))) (when (py-truthy (py-gt (count items) 1)) (update-tree (py-slice items 1 nil nil) (py-get (:children in-tree) (py-get items 0)) @header-table count-v)) nil))

(defn update-header "Update the header table with a node link.\n\nArgs:\n    node_to_test: The node to be updated in the header table.\n    target_node: The node to link to.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> node1 = TreeNode(\"A\", 3, None)\n>>> node2 = TreeNode(\"B\", 4, None)\n>>> node1\nTreeNode('A', 3, None)\n>>> node1 = update_header(node1, node2)\n>>> node1\nTreeNode('A', 3, None)\n>>> node1.node_link\nTreeNode('B', 4, None)\n>>> node2.node_link is None\nTrue"
  [node-to-test target-node] (let [node-to-test (atom node-to-test)] (try (while (py-truthy (some? (:node-link @node-to-test))) (reset! node-to-test (:node-link @node-to-test))) (when (py-truthy (nil? (:node-link @node-to-test))) (swap! node-to-test (fn [__c] (py-assoc-in __c [:node-link] target-node)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @node-to-test})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn ascend-tree "Ascend the FP-Tree from a leaf node to its root, adding item names to the prefix\npath.\n\nArgs:\n    leaf_node: The leaf node to start ascending from.\n    prefix_path: A list to store the item as they are ascended.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n\n>>> path = []\n>>> ascend_tree(fp_tree.children['A'], path)\n>>> path # ascending from a leaf node 'A'\n['A']"
  [leaf-node prefix-path] (let [prefix-path (atom prefix-path)] (when (py-truthy (some? (:parent leaf-node))) (swap! prefix-path conj (:name leaf-node)) (ascend-tree (:parent leaf-node) @prefix-path)) nil))

(defn find-prefix-path "Find the conditional pattern base for a given base pattern.\n\nArgs:\n    base_pat: The base pattern for which to find the conditional pattern base.\n    tree_node: The node in the FP-Tree.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> len(header_table)\n4\n>>> base_pattern = frozenset(['A'])\n>>> sorted(find_prefix_path(base_pattern, fp_tree.children['A']))\n[]"
  [base-pat tree-node] (let [tree-node (atom tree-node)] (let [cond-pats (atom nil) prefix-path (atom nil)] (try (reset! cond-pats {}) (while (py-truthy (some? @tree-node)) (reset! prefix-path []) (ascend-tree @tree-node @prefix-path) (when (py-truthy (py-gt (count @prefix-path) 1)) (swap! cond-pats (fn [__c] (py-assoc-in __c [(py-set (py-slice @prefix-path 1 nil nil))] (:count @tree-node))))) (reset! tree-node (:node-link @tree-node))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @cond-pats})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn mine-tree "Mine the FP-Tree recursively to discover frequent itemsets.\n\nArgs:\n    in_tree: The FP-Tree to mine.\n    header_table: The header table dictionary with item information.\n    min_sup: The minimum support threshold.\n    pre_fix: A set of items as a prefix for the itemsets being mined.\n    freq_item_list: A list to store the frequent itemsets.\n\nExample:\n>>> data_set = [\n...    ['A', 'B', 'C'],\n...    ['A', 'C'],\n...    ['A', 'B', 'E'],\n...    ['A', 'B', 'C', 'E'],\n...    ['B', 'E']\n... ]\n>>> min_sup = 2\n>>> fp_tree, header_table = create_tree(data_set, min_sup)\n>>> fp_tree\nTreeNode('Null Set', 1, None)\n>>> frequent_itemsets = []\n>>> mine_tree(fp_tree, header_table, min_sup, set([]), frequent_itemsets)\n>>> expe_itm = [{'C'}, {'C', 'A'}, {'E'}, {'A', 'E'}, {'E', 'B'}, {'A'}, {'B'}]\n>>> all(expected in frequent_itemsets for expected in expe_itm)\nTrue"
  [& _args]
  ;; stub: original requires numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:copy" {:type :UnsupportedOperation})))

(defmethod print-method TreeNode [o w] (.write w (TreeNode-repr (atom o))))

;; NOTE: mine_tree stubbed (numpy-attr:copy)
