(ns clojure-algorithms.sorts.tim-sort
  "TimSort-like sort: detect runs, insertion-sort each, merge.
  Ported from TheAlgorithms/Python."
  (:refer-clojure :exclude [merge]))

(defn binary-search
  "Insertion index for `item` in the sorted range [start, end]
  (inclusive) of vector `v`."
  [v item start end]
  (cond
    (> start end) start
    (= start end) (if (neg? (compare item (nth v start))) start (inc start))
    :else
    (let [mid (quot (+ start end) 2)
          c (compare (nth v mid) item)]
      (cond
        (neg? c) (binary-search v item (inc mid) end)
        (pos? c) (binary-search v item start (dec mid))
        :else mid))))

(defn insertion-sort
  "Returns a sorted copy of `coll` via binary insertion sort."
  [coll]
  (let [v (vec coll)]
    (loop [a v i 1]
      (if (>= i (count a))
        a
        (let [x (nth a i)
              pos (binary-search a x 0 (dec i))]
          (recur (into (conj (subvec a 0 pos) x)
                       (into (subvec a pos i) (subvec a (inc i))))
                 (inc i)))))))

(defn merge
  "Merges sorted vectors `left` and `right` into a new sorted vector."
  [left right]
  (loop [l (seq left) r (seq right) acc (transient [])]
    (cond
      (nil? l) (persistent! (reduce conj! acc r))
      (nil? r) (persistent! (reduce conj! acc l))
      (neg? (compare (first l) (first r)))
      (recur (next l) r (conj! acc (first l)))
      :else
      (recur l (next r) (conj! acc (first r))))))

(defn tim-sort
  "Sorts `coll` TimSort-style. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (let [runs (reduce (fn [runs x]
                           (let [cur (peek runs)]
                             (if (and (seq cur)
                                      (neg? (compare x (peek cur))))
                               (conj runs [x])
                               (conj (pop runs) (conj cur x)))))
                         [[(first v)]] (rest v))]
        (reduce merge [] (map insertion-sort runs))))))
