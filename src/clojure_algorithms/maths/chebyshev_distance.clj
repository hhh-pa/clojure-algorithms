(ns clojure-algorithms.maths.chebyshev-distance
  "Port of maths/chebyshev_distance.py.")

(defn chebyshev-distance
  "Returns the Chebyshev distance (also known as the Chessboard distance)
  between two n-dimensional points.
  https://en.wikipedia.org/wiki/Chebyshev_distance"
  [point-a point-b]
  (when (not= (count point-a) (count point-b))
    (throw (ex-info "Both points must have the same dimension."
                    {:type :ValueError})))
  (reduce max (map (fn [a b] (let [d (- a b)] (if (neg? d) (- d) d)))
                   point-a point-b)))
