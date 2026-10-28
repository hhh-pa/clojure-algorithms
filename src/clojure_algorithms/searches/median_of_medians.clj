(ns clojure-algorithms.searches.median-of-medians
  "Median of medians pivot selection with deterministic quick select.

  Port of TheAlgorithms/Python searches/median_of_medians.py. Ranks for
  `quick-select` are 1-based, as in the Python source. The grouping
  quirk of the Python version (a full tail slice while i + 4 <= len) is
  preserved so results match the original exactly.")

(defn median-of-five
  "Median of `arr` (upper middle for even lengths)."
  [arr]
  (let [s (vec (sort arr))]
    (nth s (quot (count s) 2))))

(defn median-of-medians
  "Median of the medians of `arr` in groups of five."
  [arr]
  (let [v (vec arr)]
    (if (<= (count v) 5)
      (median-of-five v)
      (let [medians (loop [i 0 acc []]
                      (if (>= i (count v))
                        acc
                        (recur (+ i 5)
                               (conj acc
                                     (median-of-five
                                      (if (<= (+ i 4) (count v))
                                        (subvec v i)
                                        (subvec v i (min (+ i 5)
                                                         (count v)))))))))]
        (median-of-medians medians)))))

(defn quick-select
  "Element of rank `target` (1-based) in `arr`, or -1 when `target`
  exceeds the collection size."
  [arr target]
  (let [v (vec arr)]
    (if (> target (count v))
      -1
      (let [x (median-of-medians v)
            left (filterv #(< % x) v)
            right (into (filterv #(> % x) v)
                        (repeat (dec (count (filter #(= % x) v))) x))
            rank-x (inc (count left))]
        (cond (= rank-x target) x
              (> rank-x target) (quick-select left target)
              :else (quick-select right (- target rank-x)))))))
