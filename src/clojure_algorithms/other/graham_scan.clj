(ns clojure-algorithms.other.graham-scan
  "Graham scan convex-hull algorithm.

  Port of TheAlgorithms/Python `other/graham_scan.py`.")

(defn angle-comparer
  "Angle in degrees from `(minx, miny)` toward `point`."
  [[x y] minx miny]
  (Math/toDegrees (Math/atan2 (- y miny) (- x minx))))

(defn check-direction
  "Direction of the turn from `starting` via `via` to `target`:
  `:left`, `:straight`, or `:right`."
  [[x0 y0] [x1 y1] [x2 y2]]
  (let [via-angle (mod (Math/toDegrees (Math/atan2 (- y1 y0) (- x1 x0))) 360)
        target-angle (mod (Math/toDegrees (Math/atan2 (- y2 y0) (- x2 x0))) 360)]
    (cond
      (> target-angle via-angle) :left
      (= target-angle via-angle) :straight
      :else :right)))

(defn graham-scan
  "Return the convex hull points of `points` (a seq of [x y] pairs)."
  [points]
  (let [points (vec points)]
    (cond
      (<= (count points) 2)
      (throw (IllegalArgumentException.
              "graham-scan: argument must contain more than 3 points."))
      (= (count points) 3)
      points
      :else
      (let [minidx (reduce (fn [best i]
                             (let [[bx by] (points best)
                                   [x y] (points i)]
                               (if (or (< y by) (and (= y by) (< x bx))) i best)))
                           0 (range (count points)))
            [minx miny] (points minidx)
            rest-pts (vec (concat (subvec points 0 minidx)
                                  (subvec points (inc minidx))))
            sorted-pts (vec (cons [minx miny]
                                  (sort-by #(angle-comparer % minx miny)
                                           rest-pts)))]
        (loop [stack (vec (take 3 sorted-pts))
               current-direction :left
               i 3]
          (if (>= i (count sorted-pts))
            stack
            (let [[stack current-direction]
                  (loop [stack stack current-direction current-direction]
                    (let [starting (nth stack (- (count stack) 2))
                          via (peek stack)
                          target (sorted-pts i)
                          next-direction (check-direction starting via target)]
                      (cond
                        (= next-direction :left)
                        [stack :left]
                        (= next-direction :straight)
                        (if (= current-direction :left)
                          [stack current-direction]
                          [(pop stack) current-direction])
                        :else
                        [(pop stack) current-direction])))]
              (recur (conj stack (sorted-pts i)) current-direction (inc i)))))))))
