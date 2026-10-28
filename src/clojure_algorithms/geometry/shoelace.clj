(ns clojure-algorithms.geometry.shoelace
  "Polygon area via the shoelace formula.")

(defn area-of-polygon
  "Returns the area of a simple planar polygon whose vertices (xs, ys)
   are ordered counter-clockwise."
  [xs ys]
  (* 0.5 (reduce + (map-indexed
                    (fn [i x]
                      (* (+ (nth ys i) (nth ys (mod (inc i) (count ys))))
                         (- x (nth xs (mod (inc i) (count xs))))))
                    xs))))
