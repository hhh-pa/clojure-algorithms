(ns clojure-algorithms.dynamic-programming.min-distance-up-bottom
  "Edit distance via memoized top-down dynamic programming.
  Ported from TheAlgorithms/Python dynamic_programming/min_distance_up_bottom.py.")

(defn min-distance-up-bottom
  "Edit distance between `word1` and `word2`."
  [word1 word2]
  (let [len1 (count word1)
        len2 (count word2)
        memo (atom {})]
    (letfn [(dist [i j]
              (cond
                (>= i len1) (- len2 j)
                (>= j len2) (- len1 i)
                (contains? @memo [i j]) (@memo [i j])
                :else (let [d (min (inc (dist (inc i) j))
                                   (inc (dist i (inc j)))
                                   (+ (if (= (nth word1 i) (nth word2 j)) 0 1)
                                      (dist (inc i) (inc j))))]
                        (swap! memo assoc [i j] d)
                        d)))]
      (dist 0 0))))
