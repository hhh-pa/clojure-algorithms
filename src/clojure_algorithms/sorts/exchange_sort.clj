(ns clojure-algorithms.sorts.exchange-sort
  "Exchange sort. Ported from TheAlgorithms/Python.")

(defn exchange-sort
  "Sorts `coll` with exchange sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 0]
      (if (>= i n)
        a
        (recur (loop [b a j (inc i)]
                 (if (>= j n)
                   b
                   (recur (if (neg? (compare (nth b j) (nth b i)))
                            (assoc b i (nth b j) j (nth b i))
                            b)
                          (inc j))))
               (inc i))))))
