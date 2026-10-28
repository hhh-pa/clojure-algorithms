(ns clojure-algorithms.sorts.msd-radix-sort
  "MSD radix sort on the binary representation of non-negative integers.
  Ported from TheAlgorithms/Python.")

(defn- msd-radix-sort-by-bit
  [v bit-pos]
  (if (or (zero? bit-pos) (<= (count v) 1))
    (vec v)
    (let [{zeros false ones true}
          (group-by #(= 1 (bit-and (bit-shift-right % (dec bit-pos)) 1)) v)]
      (into (msd-radix-sort-by-bit (vec zeros) (dec bit-pos))
            (msd-radix-sort-by-bit (vec ones) (dec bit-pos))))))

(defn- bit-length
  [x]
  (count (Long/toBinaryString (long x))))

(defn msd-radix-sort
  "Sorts non-negative integers `coll` with MSD radix sort.
  Returns a sorted vector. Throws IllegalArgumentException on negatives."
  [coll]
  (let [v (vec coll)]
    (cond
      (empty? v) []
      (neg? (apply min v))
      (throw (IllegalArgumentException. "All numbers must be positive"))
      :else
      (msd-radix-sort-by-bit v (apply max (map bit-length v))))))

(defn msd-radix-sort-inplace
  "Sorts non-negative integers `coll` with MSD radix sort.
  Clojure data is immutable, so this returns a new sorted vector
  rather than sorting in place."
  [coll]
  (msd-radix-sort coll))
