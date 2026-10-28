(ns clojure-algorithms.sorts.bitonic-sort
  "Bitonic sort. Only works when the input size is a power of 2.
  Direction 1 = ascending, 0 = descending.
  Ported from TheAlgorithms/Python.")

(defn comp-and-swap
  "Compares elements at `i` and `j` of vector `v` and swaps them when they
  disagree with `direction` (1 ascending, 0 descending). Returns the vector."
  [v i j direction]
  (let [a (nth v i) b (nth v j)]
    (if (or (and (= direction 1) (neg? (compare b a)))
            (and (= direction 0) (neg? (compare a b))))
      (assoc v i b j a)
      v)))

(defn bitonic-merge
  "Recursively sorts the bitonic sequence of `length` elements starting at
  `low` in vector `v` in the given `direction`. Returns the vector."
  [v low length direction]
  (if (<= length 1)
    v
    (let [middle (quot length 2)
          v2 (reduce (fn [a i] (comp-and-swap a i (+ i middle) direction))
                     v (range low (+ low middle)))]
      (-> v2
          (bitonic-merge low middle direction)
          (bitonic-merge (+ low middle) middle direction)))))

(defn bitonic-sort
  "Sorts `length` elements of `coll` starting at `low` in `direction`
  (1 ascending, 0 descending). Returns a vector."
  ([coll] (bitonic-sort coll 0 (count coll) 1))
  ([coll low length direction]
   (let [v (vec coll)]
     (if (<= length 1)
       v
       (let [middle (quot length 2)]
         (-> v
             (bitonic-sort low middle 1)
             (bitonic-sort (+ low middle) middle 0)
             (bitonic-merge low length direction)))))))
