(ns clojure-algorithms.graphs.minimum-path-sum
  "Minimum top-left to bottom-right path sum in a grid.")

(defn fill-row
  "Fills one DP row from the row above."
  [current-row row-above]
  (reduce (fn [acc [i cell]]
            (conj acc (+ cell (if (zero? i)
                                (nth row-above 0)
                                (min (peek acc) (nth row-above i))))))
          [] (map-indexed vector current-row)))

(defn min-path-sum
  "Returns the minimum path sum moving only down/right."
  [grid]
  (when (or (nil? grid) (empty? grid) (empty? (first grid)))
    (throw (IllegalArgumentException.
            "The grid does not contain the appropriate information")))
  (let [first-row (vec (reductions + (first grid)))]
    (loop [rows (rest grid) above first-row]
      (if (empty? rows)
        (peek above)
        (recur (rest rows) (fill-row (first rows) above))))))
