(ns clojure-algorithms.strings.boyer-moore-horspool
  "Boyer-Moore-Horspool string search (bad-character shifts).
  Ported from TheAlgorithms/Python.")

(defn build-shift-table
  "Bad-character shift table for `pattern`."
  [pattern]
  (let [m (count pattern)]
    (into {} (map (fn [i] [(nth pattern i) (- m 1 i)])
                  (range (dec m))))))

(defn boyer-moore-horspool-search
  "Index of the first occurrence of `pattern` in `text`, or -1.
  An empty pattern matches at 0."
  [text pattern]
  (let [m (count pattern)
        n (count text)]
    (cond
      (zero? m) 0
      (> m n) -1
      :else
      (let [shifts (build-shift-table pattern)]
        (loop [skip 0]
          (if (> (+ skip m) n)
            -1
            (let [i (loop [k (dec m)]
                      (if (and (>= k 0)
                               (= (nth pattern k) (nth text (+ skip k))))
                        (recur (dec k))
                        k))]
              (if (neg? i)
                skip
                (recur (+ skip (get shifts (nth text (+ skip m -1)) m)))))))))))

(defn boyer-moore-horspool-search-all
  "Every starting index of `pattern` in `text` (overlapping included)."
  [text pattern]
  (let [m (count pattern)
        n (count text)]
    (cond
      (zero? m) (vec (range (inc n)))
      (> m n) []
      :else
      (let [shifts (build-shift-table pattern)]
        (loop [skip 0 matches []]
          (if (> (+ skip m) n)
            matches
            (let [i (loop [k (dec m)]
                      (if (and (>= k 0)
                               (= (nth pattern k) (nth text (+ skip k))))
                        (recur (dec k))
                        k))]
              (if (neg? i)
                (recur (inc skip) (conj matches skip))
                (recur (+ skip (get shifts (nth text (+ skip m -1)) m))
                       matches)))))))))
