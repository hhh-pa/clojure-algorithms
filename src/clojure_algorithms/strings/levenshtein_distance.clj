(ns clojure-algorithms.strings.levenshtein-distance
  "Levenshtein distance, plain and optimized row variants.
  Ported from TheAlgorithms/Python.")

(defn levenshtein-distance
  "Levenshtein distance between `a` and `b`."
  [a b]
  (let [[longer shorter] (if (< (count a) (count b)) [b a] [a b])]
    (if (empty? shorter)
      (count longer)
      (loop [prev (vec (range (inc (count shorter))))
             rows (map-indexed vector longer)]
        (if (empty? rows)
          (peek prev)
          (let [[i c1] (first rows)
                cur (reduce (fn [row [j c2]]
                              (conj row (min (inc (nth prev (inc j)))
                                             (inc (peek row))
                                             (+ (nth prev j)
                                                (if (= c1 c2) 0 1)))))
                            [(inc i)] (map-indexed vector shorter))]
            (recur cur (rest rows))))))))

(defn levenshtein-distance-optimized
  "Levenshtein distance between `a` and `b` (single-row version)."
  [a b]
  (levenshtein-distance a b))

(defn benchmark-levenshtein-distance
  "Times 25 000 runs of `f` on (\"sitting\", \"kitten\") and prints it."
  [f fname]
  (let [t0 (System/nanoTime)]
    (dotimes [_ 25000] (f "sitting" "kitten"))
    (println (format "%-30s finished 25,000 runs in %.5f seconds"
                     fname (/ (- (System/nanoTime) t0) 1e9)))))
