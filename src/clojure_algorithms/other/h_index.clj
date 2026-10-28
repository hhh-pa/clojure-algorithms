(ns clojure-algorithms.other.h-index
  "H-index computation for citation counts.

  Port of TheAlgorithms/Python `other/h_index.py`.")

(defn h-index
  "Return the h-index of `citations`, a collection of non-negative ints."
  [citations]
  (when (or (not (sequential? citations))
            (not (every? #(and (integer? %) (>= % 0)) citations)))
    (throw (IllegalArgumentException.
            "The citations should be a list of non negative integers.")))
  (let [sorted (vec (sort citations))
        n (count sorted)]
    (loop [i 0]
      (if (>= i n)
        n
        (if (<= (sorted (- n 1 i)) i)
          i
          (recur (inc i)))))))
