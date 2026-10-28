(ns clojure-algorithms.sorts.insertion-sort
  "Insertion sort (stable). Ported from TheAlgorithms/Python.")

(defn insertion-sort
  "Sorts `coll` with insertion sort (stable). Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (loop [a v i 1]
      (if (>= i (count a))
        a
        (let [x (nth a i)]
          (recur (loop [b a j i]
                   (if (and (pos? j)
                            (neg? (compare x (nth b (dec j)))))
                     (recur (assoc b j (nth b (dec j))) (dec j))
                     (assoc b j x)))
                 (inc i)))))))
