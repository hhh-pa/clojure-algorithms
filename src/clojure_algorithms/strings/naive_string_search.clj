(ns clojure-algorithms.strings.naive-string-search
  "Naive O(n*m) string search. Ported from TheAlgorithms/Python.")

(defn naive-pattern-search
  "All starting indices of `pattern` in `s`."
  [s pattern]
  (let [m (count pattern)]
    (vec (filter (fn [i]
                   (= (subs s i (min (+ i m) (count s))) pattern))
                 (range (inc (- (count s) m)))))))
