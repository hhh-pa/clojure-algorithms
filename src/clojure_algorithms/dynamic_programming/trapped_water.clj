(ns clojure-algorithms.dynamic-programming.trapped-water
  "Rainwater trapped by an elevation map (left/right maxima DP).
  Ported from TheAlgorithms/Python dynamic_programming/trapped_water.py.")

(defn trapped-rainwater
  "Total trapped water over bar heights `heights`."
  [heights]
  (let [h (vec heights)]
    (cond
      (empty? h) 0
      (some neg? h) (throw (IllegalArgumentException.
                            "No height can be negative"))
      :else (let [n (count h)
                  left-max (vec (reductions max h))
                  right-max (vec (reverse (reductions max (reverse h))))]
              (reduce + (map (fn [l r height] (- (min l r) height))
                             left-max right-max h))))))
