(ns clojure-algorithms.divide-and-conquer.mergesort
  "Merge sort: O(n log n) divide-and-conquer sorting.
  Ported from TheAlgorithms/Python divide_and_conquer/mergesort.py.")

(defn merge-halves
  "Merge two sorted vectors into one sorted vector."
  [left-half right-half]
  (loop [l left-half r right-half acc []]
    (cond
      (empty? l) (into acc r)
      (empty? r) (into acc l)
      (< (first l) (first r)) (recur (rest l) r (conj acc (first l)))
      :else (recur l (rest r) (conj acc (first r))))))

(defn merge-sort
  "Return a sorted vector of the elements in `coll`."
  [coll]
  (let [arr (vec coll)]
    (if (<= (count arr) 1)
      arr
      (let [mid (quot (count arr) 2)]
        (merge-halves (merge-sort (subvec arr 0 mid))
                      (merge-sort (subvec arr mid)))))))
