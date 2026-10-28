(ns clojure-algorithms.maths.padovan-sequence
  "Padovan sequence: P(0) = P(1) = P(2) = 1, P(n) = P(n-2) + P(n-3).")

(defn padovan-sequence
  "Return the n-th term of the Padovan Sequence."
  [n]
  (when (or (not (integer? n)) (neg? n))
    (throw (ex-info "Input must be a non-negative integer." {:type :ValueError})))
  (if (<= n 2)
    1
    (loop [i 3
           p-prev-3 1
           p-prev-2 1
           p-prev-1 1]
      (if (> i n)
        p-prev-1
        (recur (inc i) p-prev-2 p-prev-1 (+ p-prev-2 p-prev-3))))))
