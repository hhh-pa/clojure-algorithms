(ns clojure-algorithms.divide-and-conquer.peak
  "Peak of a unimodal sequence via divide and conquer.

  A unimodal sequence rises to an index p and falls afterwards.
  Ported from TheAlgorithms/Python divide_and_conquer/peak.py.")

(defn peak
  "Return the peak value of unimodal vector `coll`."
  [coll]
  (let [lst (vec coll)
        m (quot (count lst) 2)
        three (subvec lst (dec m) (+ m 2))]
    (cond
      (and (> (three 1) (three 0)) (> (three 1) (three 2)))
      (three 1)
      (< (three 0) (three 2))
      (peak (subvec lst (if (= (count (subvec lst 0 m)) 2) (dec m) m)))
      :else
      (peak (subvec lst 0 (if (= (count (subvec lst 0 m)) 2) (inc m) m))))))
