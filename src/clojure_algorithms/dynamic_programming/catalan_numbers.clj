(ns clojure-algorithms.dynamic-programming.catalan-numbers
  "Catalan numbers C(0)..C(n) via the recurrence
  C(n) = sum(C(i) * C(n-1-i)).
  Ported from TheAlgorithms/Python dynamic_programming/catalan_numbers.py.")

(defn catalan-numbers
  "Vector of Catalan numbers from 0 through `upper-limit`."
  [upper-limit]
  (when (neg? upper-limit)
    (throw (IllegalArgumentException.
            "Limit for the Catalan sequence must be \u2265 0")))
  (loop [i 2 catalan (cond-> (assoc (vec (repeat (inc upper-limit) 0)) 0 1)
                       (> upper-limit 0) (assoc 1 1))]
    (if (> i upper-limit)
      catalan
      (recur (inc i)
             (assoc catalan i (reduce + (for [j (range i)]
                                          (* (catalan j)
                                             (catalan (- i j 1))))))))))
