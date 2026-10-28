(ns clojure-algorithms.machine-learning.apriori-algorithm
  "Apriori Algorithm is a Association rule mining technique, also known as market basket\nanalysis, aims to discover interesting relationships or associations among a set of\nitems in a transactional or relational database.\n\nFor example, Apriori Algorithm states: \"If a customer buys item A and item B, then they\nare likely to buy item C.\"  This rule suggests a relationship between items A, B, and C,\nindicating that customers who purchased A and B are more likely to also purchase item C.\n\nWIKI: https://en.wikipedia.org/wiki/Apriori_algorithm\nExamples: https://www.kaggle.com/code/earthian/apriori-association-rules-mining"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare load-data prune apriori)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-ddget py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-sorted py-list py-combinations py-counter py-float-str py-quote py-repr)

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

(defn- py-ddget [c k d]
  (if (map? c) (get c k d) (py-get c k)))

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

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

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

(defn- py-combinations [xs r]
  (let [v (vec xs) n (count v)]
    (cond (neg? r) (throw (ex-info "r must be non-negative" {:type :ValueError}))
          (zero? r) [[]]
          (> r n) []
          :else (vec (mapcat (fn [i]
                               (map #(vec (cons (nth v i) %))
                                    (py-combinations (subvec v (inc i)) (dec r))))
                             (range (- (inc n) r)))))))

(defn- py-counter [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (string? x) (frequencies (map str (seq x)))
        :else (frequencies (py-iter x))))

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

(defn load-data "Returns a sample transaction dataset.\n\n>>> load_data()\n[['milk'], ['milk', 'butter'], ['milk', 'bread'], ['milk', 'bread', 'chips']]"
  [] [["milk"] ["milk" "butter"] ["milk" "bread"] ["milk" "bread" "chips"]])

(defn prune "Prune candidate itemsets that are not frequent.\nThe goal of pruning is to filter out candidate itemsets that are not frequent.  This\nis done by checking if all the (k-1) subsets of a candidate itemset are present in\nthe frequent itemsets of the previous iteration (valid subsequences of the frequent\nitemsets from the previous iteration).\n\nPrunes candidate itemsets that are not frequent.\n\n>>> itemset = ['X', 'Y', 'Z']\n>>> candidates = [['X', 'Y'], ['X', 'Z'], ['Y', 'Z']]\n>>> prune(itemset, candidates, 2)\n[['X', 'Y'], ['X', 'Z'], ['Y', 'Z']]\n\n>>> itemset = ['1', '2', '3', '4']\n>>> candidates = ['1', '2', '4']\n>>> prune(itemset, candidates, 3)\n[]"
  [itemset candidates length] (let [candidate (atom nil) is-subsequence (atom nil) item (atom nil) item-tuple (atom nil) itemset-counter (atom nil) pruned (atom nil)] (try (reset! itemset-counter (py-counter (doall (for [item (py-iter itemset)] (py-list item))))) (reset! pruned []) (doseq [__i1 (py-iter candidates)] (reset! candidate __i1) (reset! is-subsequence true) (try (doseq [__i2 (py-iter @candidate)] (reset! item __i2) (reset! item-tuple (py-list @item)) (when (py-truthy (let [__b3 (not (py-in @item-tuple @itemset-counter))] (if (py-truthy __b3) __b3 (py-lt (py-ddget @itemset-counter @item-tuple 0) (py-sub length 1))))) (reset! is-subsequence false) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (when (py-truthy @is-subsequence) (swap! pruned conj @candidate))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @pruned})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn apriori "Returns a list of frequent itemsets and their support counts.\n\n>>> data = [['A', 'B', 'C'], ['A', 'B'], ['A', 'C'], ['A', 'D'], ['B', 'C']]\n>>> apriori(data, 2)\n[(['A', 'B'], 1), (['A', 'C'], 2), (['B', 'C'], 2)]\n\n>>> data = [['1', '2', '3'], ['1', '2'], ['1', '3'], ['1', '4'], ['2', '3']]\n>>> apriori(data, 3)\n[]"
  [data min-support] (let [candidate (atom nil) counts (atom nil) frequent-itemsets (atom nil) i (atom nil) item (atom nil) itemset (atom nil) j (atom nil) length (atom nil) transaction (atom nil)] (try (reset! itemset (vec (for [transaction (py-iter data)] (py-list transaction)))) (reset! frequent-itemsets []) (reset! length 1) (while (py-truthy @itemset) (reset! counts (py-mul [0] (count @itemset))) (doseq [__i1 (py-iter data)] (reset! transaction __i1) (doseq [[__i2 __i3] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @itemset))] (reset! j __i2) (reset! candidate __i3) (when (py-truthy (every? py-truthy (doall (for [item (py-iter @candidate)] (py-in item @transaction))))) (swap! counts (fn [__c] (py-assoc-in __c [@j] (py-add (py-get-in __c [@j]) 1))))))) (reset! itemset (vec (for [[i item] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @itemset)) :when (py-truthy (py-ge (py-get @counts i) min-support))] item))) (doseq [[__i4 __i5] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @itemset))] (reset! i __i4) (reset! item __i5) (swap! frequent-itemsets conj [(py-sorted @item) (py-get @counts @i)])) (swap! length py-add 1) (reset! itemset (prune @itemset (py-list (py-combinations @itemset @length)) @length))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @frequent-itemsets})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
