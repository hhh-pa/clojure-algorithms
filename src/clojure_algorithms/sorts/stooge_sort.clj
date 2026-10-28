(ns clojure-algorithms.sorts.stooge-sort
  "Stooge sort. Ported from TheAlgorithms/Python.")

(defn stooge
  "Recursively stooge-sorts the inclusive range [i, h] of vector `v`.
  Returns the updated vector."
  [v i h]
  (if (>= i h)
    v
    (let [a (if (neg? (compare (nth v h) (nth v i)))
              (assoc v i (nth v h) h (nth v i))
              v)]
      (if (> (- h i) 1)
        (let [t (quot (- h i -1) 3)]
          (-> a
              (stooge i (- h t))
              (stooge (+ i t) h)
              (stooge i (- h t))))
        a))))

(defn stooge-sort
  "Sorts `coll` with stooge sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v) [] (stooge v 0 (dec (count v))))))
