(ns clojure-algorithms.maths.check-polygon
  "Port of maths/check_polygon.py")

(defn check-polygon
  "Takes a list of possible side lengths and determines whether a
two-dimensional polygon with such side lengths can exist.
Returns whether the largest side is shorter than the sum of the rest.
Wiki: https://en.wikipedia.org/wiki/Triangle_inequality"
  [nums]
  (when (< (count nums) 3)
    (throw (ex-info "Monogons and Digons are not polygons in the Euclidean space"
                    {:type :ValueError})))
  (when (some #(<= % 0) nums)
    (throw (ex-info "All values must be greater than 0" {:type :ValueError})))
  (let [sorted-nums (sort nums)]
    (< (last sorted-nums) (reduce +' 0 (butlast sorted-nums)))))
