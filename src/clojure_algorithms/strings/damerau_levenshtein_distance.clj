(ns clojure-algorithms.strings.damerau-levenshtein-distance
  "Damerau-Levenshtein edit distance (with adjacent transposition).
  Ported from TheAlgorithms/Python.")

(defn damerau-levenshtein-distance
  "Edit distance between `a` and `b` counting insert/delete/substitute
  and adjacent transposition as 1."
  [a b]
  (let [n (count a)
        m (count b)
        dp (vec (for [i (range (inc n))]
                  (vec (for [j (range (inc m))]
                         (cond (zero? i) j
                               (zero? j) i
                               :else 0)))))]
    (loop [d dp i 1]
      (if (> i n)
        (get-in d [n m])
        (recur (loop [d2 d j 1]
                 (if (> j m)
                   d2
                   (let [cost (if (= (nth a (dec i)) (nth b (dec j))) 0 1)
                         best (min (inc (get-in d2 [(dec i) j]))
                                   (inc (get-in d2 [i (dec j)]))
                                   (+ (get-in d2 [(dec i) (dec j)]) cost))
                         best2 (if (and (> i 1) (> j 1)
                                        (= (nth a (dec i)) (nth b (- j 2)))
                                        (= (nth a (- i 2)) (nth b (dec j))))
                                 (min best
                                      (+ (get-in d2 [(- i 2) (- j 2)]) cost))
                                 best)]
                     (recur (assoc-in d2 [i j] best2) (inc j)))))
               (inc i))))))
