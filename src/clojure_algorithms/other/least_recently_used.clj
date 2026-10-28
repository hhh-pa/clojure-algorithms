(ns clojure-algorithms.other.least-recently-used
  "Least-recently-used page cache (keys only, functional).

  Port of TheAlgorithms/Python `other/least_recently_used.py`.
  A cache is a map `{:capacity n :order [...]}` with most-recently-used
  keys first; `refer` returns the updated cache."
  (:refer-clojure :exclude [refer]))

(defn make-lru
  "Create an empty LRU cache holding at most `n` keys."
  [n]
  (when (neg? n)
    (throw (IllegalArgumentException. "n should be an integer greater than 0.")))
  {:capacity n :order []})

(defn refer
  "Return the cache after referencing key `x`: move it to the front,
  evicting the least-recently-used key when full."
  [cache x]
  (let [order (vec (remove #(= % x) (:order cache)))
        order (vec (cons x order))
        cap (:capacity cache)]
    (assoc cache :order (if (and (pos? cap) (> (count order) cap))
                          (vec (butlast order))
                          order))))

(defn cache-keys
  "Keys from most- to least-recently-used."
  [cache]
  (:order cache))
