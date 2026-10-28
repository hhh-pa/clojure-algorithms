(ns clojure-algorithms.sorts.cyclic-sort
  "Cyclic sort for n unique integers in the range 1..n. O(n) time.
  Ported from TheAlgorithms/Python.")

(defn cyclic-sort
  "Sorts `coll` (n unique integers from 1 to n) with cyclic sort.
  Returns a sorted vector. Throws IllegalArgumentException on duplicates
  or out-of-range values."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [seen #{} xs v]
      (when (seq xs)
        (let [num (first xs)]
          (when (contains? seen num)
            (throw (IllegalArgumentException.
                    (str "All numbers must be unique, got " num))))
          (when (or (< num 1) (> num n))
            (throw (IllegalArgumentException.
                    (str "All numbers must be in range 1 to " n ", got " num))))
          (recur (conj seen num) (rest xs)))))
    (loop [a v i 0]
      (if (>= i n)
        a
        (let [correct (dec (nth a i))]
          (if (= i correct)
            (recur a (inc i))
            (recur (assoc a i (nth a correct) correct (nth a i)) i)))))))
