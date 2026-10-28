(ns clojure-algorithms.divide-and-conquer.minimum-element-of-array
  "ith smallest element via randomized partition (quickselect).

  A divide-and-conquer demonstration: like quicksort it partitions
  recursively, but only follows one side. Expected O(n) time.
  Ported from TheAlgorithms/Python
  divide_and_conquer/minimum_element_of_array.py.")

(defn partition-array
  "Lomuto partition of vector `arr` between `lo` and `hi` (inclusive).
  Returns [partitioned-vector pivot-index]."
  [arr lo hi]
  (loop [a (vec arr) i (dec lo) j lo]
    (if (>= j hi)
      [(assoc a (inc i) (a hi) hi (a (inc i))) (inc i)]
      (if (<= (a j) (a hi))
        (recur (assoc a (inc i) (a j) j (a (inc i))) (inc i) (inc j))
        (recur a i (inc j))))))

(defn randomized-partition
  "Partition with a random pivot. Returns [partitioned-vector pivot-index]."
  [arr lo hi]
  (let [a (vec arr)
        r (+ lo (rand-int (inc (- hi lo))))
        swapped (assoc a r (a hi) hi (a r))]
    (partition-array swapped lo hi)))

(defn selection-sort
  "Return the `k`th smallest element (1-based) of `coll`.
  Returns [] for an empty collection (matching the Python original)."
  [coll lo hi k]
  (let [arr (vec coll)]
    (cond
      (empty? arr) []
      (= lo hi) (arr lo)
      :else (let [[a q] (randomized-partition arr lo hi)
                  rank (inc (- q lo))]
              (cond
                (= k rank) (a q)
                (< k rank) (selection-sort a lo (dec q) k)
                :else (selection-sort a (inc q) hi (- k rank)))))))
