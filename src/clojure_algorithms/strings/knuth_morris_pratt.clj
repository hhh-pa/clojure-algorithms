(ns clojure-algorithms.strings.knuth-morris-pratt
  "Knuth-Morris-Pratt pattern search.
  Ported from TheAlgorithms/Python.")

(defn get-failure-array
  "KMP failure (prefix) array for `pattern`."
  [pattern]
  (if (empty? pattern)
    []
    (loop [failure [0] i 0 j 1]
      (if (>= j (count pattern))
        failure
        (cond
          (= (nth pattern i) (nth pattern j))
          (recur (conj failure (inc i)) (inc i) (inc j))

          (> i 0)
          (recur failure (nth failure (dec i)) j)

          :else
          (recur (conj failure 0) 0 (inc j)))))))

(defn knuth-morris-pratt
  "Index of the first occurrence of `pattern` in `text`, or -1."
  [text pattern]
  (if (empty? pattern)
    0
    (let [failure (get-failure-array pattern)]
      (loop [i 0 j 0]
        (if (>= i (count text))
          -1
          (cond
            (= (nth pattern j) (nth text i))
            (if (= j (dec (count pattern)))
              (- i j)
              (recur (inc i) (inc j)))

            (> j 0)
            (recur i (nth failure (dec j)))

            :else
            (recur (inc i) 0)))))))
