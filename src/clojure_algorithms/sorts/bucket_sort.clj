(ns clojure-algorithms.sorts.bucket-sort
  "Bucket sort for numeric collections.
  Ported from TheAlgorithms/Python.")

(defn bucket-sort
  "Sorts numeric `coll` by distributing values into `bucket-count` buckets,
  sorting each bucket and concatenating. Returns a sorted vector.
  Throws IllegalArgumentException when `bucket-count` is not an integer."
  ([coll] (bucket-sort coll 10))
  ([coll bucket-count]
   (when-not (integer? bucket-count)
     (throw (IllegalArgumentException. "bucket_count must be an integer")))
   (let [v (vec coll)]
     (cond
       (or (empty? v) (<= bucket-count 0)) []
       (apply = v) v
       :else
       (let [lo (apply min v)
             hi (apply max v)]
         (if (= lo hi)
           v
           (let [size (/ (- hi lo) bucket-count)
                 buckets (reduce (fn [bs x]
                                   (let [idx (min (int (/ (- x lo) size))
                                                  (dec bucket-count))]
                                     (update bs idx conj x)))
                                 (vec (repeat bucket-count []))
                                 v)]
             (vec (mapcat sort buckets)))))))))
