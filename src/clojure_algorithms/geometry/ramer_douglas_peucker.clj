(ns clojure-algorithms.geometry.ramer-douglas-peucker
  "Polyline simplification via the Ramer-Douglas-Peucker algorithm.")

(defn euclidean-distance
  "Euclidean distance between 2D points [x y]."
  [[ax ay] [bx by]]
  (Math/hypot (- bx ax) (- by ay)))

(defn perpendicular-distance
  "Distance from point to the line segment between line-start and line-end."
  [[px py] [ax ay] [bx by]]
  (let [dx (- bx ax) dy (- by ay)
        len-sq (+ (* dx dx) (* dy dy))]
    (if (zero? len-sq)
      (euclidean-distance [px py] [ax ay])
      (let [t (-> (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len-sq)
                  (max 0.0) (min 1.0))]
        (Math/hypot (- px (+ ax (* t dx))) (- py (+ ay (* t dy))))))))

(defn ramer-douglas-peucker
  "Simplifies polyline pts keeping every deviation above epsilon.
   First and last points are always preserved."
  [pts epsilon]
  (when (neg? epsilon)
    (throw (IllegalArgumentException.
            (str "epsilon must be non-negative, got " epsilon))))
  (if (< (count pts) 3)
    (vec pts)
    (let [n (count pts)]
      (loop [stack [[0 (dec n)]] keep #{0 (dec n)}]
        (if (empty? stack)
          (mapv pts (sort keep))
          (let [[start end] (peek stack)
                rest-stack (pop stack)]
            (if (< (- end start) 2)
              (recur rest-stack keep)
              (let [[max-dist max-idx]
                    (reduce (fn [[bd bi] i]
                              (let [d (perpendicular-distance
                                       (nth pts i) (nth pts start) (nth pts end))]
                                (if (> d bd) [d i] [bd bi])))
                            [0.0 start] (range (inc start) end))]
                (if (> max-dist epsilon)
                  (recur (conj rest-stack [start max-idx] [max-idx end])
                         (conj keep max-idx))
                  (recur rest-stack keep))))))))))
