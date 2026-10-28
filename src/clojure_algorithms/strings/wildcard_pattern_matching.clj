(ns clojure-algorithms.strings.wildcard-pattern-matching
  "Full-string regex matching with '.' and '*' (bottom-up DP).
  Ported from TheAlgorithms/Python.")

(defn match-pattern
  "True when the whole of `s` matches `pattern`, where '.' matches any
  single char and '*' matches zero or more of the preceding element."
  [s pattern]
  (let [n (inc (count s))
        m (inc (count pattern))
        dp (vec (for [i (range n)]
                  (vec (for [j (range m)]
                         (cond (and (zero? i) (zero? j)) true
                               (zero? i) false
                               (zero? j) false
                               :else false)))))
        dp2 (reduce (fn [d j]
                      (if (and (>= j 2) (= \* (nth pattern (dec j))))
                        (assoc-in d [0 j] (get-in d [0 (- j 2)]))
                        d))
                    dp (range 1 m))]
    (loop [d dp2 i 1]
      (if (>= i n)
        (boolean (get-in d [(dec n) (dec m)]))
        (recur (loop [d2 d j 1]
                 (if (>= j m)
                   d2
                   (let [pc (nth pattern (dec j))
                         v (cond
                             (or (= (nth s (dec i)) pc) (= pc \.))
                             (get-in d2 [(dec i) (dec j)])

                             (= pc \*)
                             (if (< j 2)
                               false
                               (cond
                                 (get-in d2 [i (- j 2)]) true
                                 (contains? #{(nth s (dec i)) \.}
                                            (nth pattern (- j 2)))
                                 (get-in d2 [(dec i) j])
                                 :else false))

                             :else false)]
                     (recur (assoc-in d2 [i j] (boolean v)) (inc j)))))
               (inc i))))))
