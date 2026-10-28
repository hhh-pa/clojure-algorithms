(ns clojure-algorithms.greedy-methods.minimum-waiting-time
  "Minimum total waiting time for queries served in shortest-first order.")

(defn minimum-waiting-time
  "Returns the minimum total waiting time for the given query durations."
  [queries]
  (let [n (count queries)]
    (if (<= n 1)
      0
      (reduce + (map-indexed (fn [i q] (* q (- n i 1)))
                             (sort queries))))))
