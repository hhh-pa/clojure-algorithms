(ns clojure-algorithms.maths.derangement
  "A Python implementation for finding number of
derangements possible for k objects
https://en.wikipedia.org/wiki/Derangement")

(defn derangement
  "Calculate the number of derangements of k objects via the recurrence
  F(n) = (n - 1) * (F(n - 1) + F(n - 2)). Throws :ValueError for
  negative input."
  [objects]
  (when-not (number? objects)
    (throw (ex-info "k must be a non-negative integer. Retry"
                    {:type :TypeError})))
  (when (< objects 0)
    (throw (ex-info "k must be a non-negative integer. Retry"
                    {:type :ValueError})))
  (when-not (integer? objects)
    (throw (ex-info "k must be a non-negative integer. Retry"
                    {:type :TypeError})))
  (if (<= objects 1)
    0
    (loop [i 3
           derange-1 1
           derange-2 0
           answer 1]
      (if (> i objects)
        answer
        (let [a (*' (dec i) (+' derange-1 derange-2))]
          (recur (inc i) a derange-1 a))))))
