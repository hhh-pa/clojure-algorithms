(ns clojure-algorithms.sorts.recursive-insertion-sort
  "Recursive insertion sort. Ported from TheAlgorithms/Python.")

(defn insert-next
  "Bubbles the element at `index` rightward while it is smaller than its
  left neighbor. Returns the updated vector."
  [v index]
  (if (or (>= index (count v))
          (not (neg? (compare (nth v index) (nth v (dec index))))))
    v
    (recur (assoc v (dec index) (nth v index) index (nth v (dec index)))
           (inc index))))

(defn rec-insertion-sort
  "Sorts the first `n` elements of `v` with recursive insertion sort.
  Returns the sorted vector."
  [v n]
  (let [a (vec v)]
    (if (or (<= (count a) 1) (<= n 1))
      a
      (rec-insertion-sort (insert-next a (dec n)) (dec n)))))

(defn recursive-insertion-sort
  "Sorts all of `coll` with recursive insertion sort."
  [coll]
  (let [v (vec coll)]
    (rec-insertion-sort v (count v))))
