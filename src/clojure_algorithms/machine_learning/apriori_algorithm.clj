(ns clojure-algorithms.machine-learning.apriori-algorithm
  "Apriori association-rule mining (market basket analysis): discover
  relationships such as \"customers who buy A and B also buy C\" among
  items in a transaction database.

  Reference: https://en.wikipedia.org/wiki/Apriori_algorithm")

(defn load-data
  "Sample transaction dataset."
  []
  [["milk"] ["milk" "butter"] ["milk" "bread"] ["milk" "bread" "chips"]])

(defn- tuple-key
  "Python tuple() of an item: vectors stay vectors, strings become a vector
  of their characters, matching tuple(item) on both levels."
  [x]
  (if (sequential? x) (vec x) (mapv str (seq (str x)))))

(defn- candidate-items
  "Elements of a candidate: vectors iterate directly, strings iterate by
  character, matching Python's for-in semantics."
  [candidate]
  (if (sequential? candidate) candidate (map str (seq (str candidate)))))

(defn prune
  "Keep candidates whose every item-tuple is frequent in itemset (count of at
  least length - 1).

  >>> (prune [\"X\" \"Y\" \"Z\"] [[\"X\" \"Y\"] [\"X\" \"Z\"] [\"Y\" \"Z\"]] 2)
  [[\"X\" \"Y\"] [\"X\" \"Z\"] [\"Y\" \"Z\"]]"
  [itemset candidates length]
  (let [counter (frequencies (map tuple-key itemset))]
    (vec (filter (fn [candidate]
                   (every? (fn [item]
                             (let [k (tuple-key item)]
                               (and (contains? counter k)
                                    (>= (get counter k) (dec length)))))
                           (candidate-items candidate)))
                 candidates))))

(defn- combinations
  "All k-combinations of coll in index order (like itertools.combinations)."
  [coll k]
  (let [v (vec coll)
        n (count v)]
    (cond (<= k 0) [[]]
          (> k n) []
          :else (vec (mapcat (fn [i]
                               (map #(into [(nth v i)] %)
                                    (combinations (subvec v (inc i)) (dec k))))
                             (range 0 (inc (- n k))))))))

(defn apriori
  "Frequent itemsets of data with support >= min-support, as
  [sorted-itemset support] pairs.

  NOTE: support counts are read back with the filtered indices, exactly as
  the upstream implementation does (it enumerates the pruned itemset while
  indexing the pre-pruning counts), so counts for pruned-away neighbours
  shift positions. This quirk is preserved for behaviour parity:
  >>> (apriori [[\"A\" \"B\" \"C\"] [\"A\" \"B\"] [\"A\" \"C\"] [\"A\" \"D\"] [\"B\" \"C\"]] 2)
  [[[\"A\" \"B\"] 1] [[\"A\" \"C\"] 2] [[\"B\" \"C\"] 2]]"
  [data min-support]
  (loop [itemset (mapv vec data)
         frequent []
         length 1]
    (if (empty? itemset)
      frequent
      (let [counts (mapv (fn [candidate]
                           (count (filter (fn [transaction]
                                            (every? #(some #{%} transaction)
                                                    candidate))
                                          data)))
                         itemset)
            kept (vec (keep-indexed (fn [i item]
                                      (when (>= (nth counts i) min-support) item))
                                    itemset))
            frequent (into frequent
                           (map-indexed (fn [i item]
                                          [(vec (sort item)) (nth counts i)])
                                        kept))
            length (inc length)]
        (recur (prune kept (combinations kept length) length)
               frequent
               length)))))
