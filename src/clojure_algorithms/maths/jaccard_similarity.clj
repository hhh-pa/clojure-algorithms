(ns clojure-algorithms.maths.jaccard-similarity
  "The Jaccard similarity coefficient is a commonly used indicator of the
  similarity between two sets. Let U be a set and A and B be subsets of U,
  then the Jaccard index/similarity is defined to be the ratio of the number
  of elements of their intersection and the number of elements of their union.

  https://en.wikipedia.org/wiki/Jaccard_index"
  (:require [clojure.set :as set]))

(defn jaccard-similarity
  "Returns the Jaccard similarity (intersection over union) of `set-a` and
  `set-b`, which must either both be sets or both be sequential collections.
  When `alternative-union` is true, the union is taken as the sum of the
  item counts instead."
  ([set-a set-b] (jaccard-similarity set-a set-b false))
  ([set-a set-b alternative-union]
   (cond
     (and (set? set-a) (set? set-b))
     (let [inter (count (set/intersection set-a set-b))
           union (if alternative-union
                   (+ (count set-a) (count set-b))
                   (count (set/union set-a set-b)))]
       (/ (double inter) union))
     (and (sequential? set-a) (sequential? set-b))
     (let [inter (count (filter #(boolean (some #{%} set-b)) set-a))]
       (if alternative-union
         (/ (double inter) (+ (count set-a) (count set-b)))
         (let [union (+ (count set-a)
                        (count (remove #(boolean (some #{%} set-a)) set-b)))]
           (/ (double inter) union))))
     :else
     (throw (ex-info (str "Set a and b must either both be sets or be either "
                          "a list or a tuple.")
                     {:type :ValueError})))))
