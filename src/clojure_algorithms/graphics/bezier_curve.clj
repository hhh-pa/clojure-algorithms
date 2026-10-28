(ns clojure-algorithms.graphics.bezier-curve
  "Bezier curve evaluation from control points. Plotting (matplotlib in
   the Python original) is intentionally omitted; this port keeps the
   pure curve mathematics.")

(defn- binomial
  "Binomial coefficient C(n, k)."
  [n k]
  (if (or (< k 0) (> k n))
    0
    (let [k (min k (- n k))]
      (loop [i 0 acc 1]
        (if (>= i k) acc
            (recur (inc i) (/ (* acc (- n i)) (inc i))))))))

(defn make-curve
  "Builds a Bezier curve from control points [[x y] ...]."
  [points] {:points (vec points) :degree (dec (count points))})

(defn basis-function
  "Weight of each control point at time t in [0, 1]."
  [{:keys [points degree]} t]
  (when-not (<= 0 t 1)
    (throw (IllegalArgumentException. "Time t must be between 0 and 1.")))
  (mapv (fn [i]
          (* (binomial degree i)
             (Math/pow (- 1 t) (- degree i))
             (Math/pow t i)))
        (range (count points))))

(defn bezier-curve-function
  "Point [x y] on the curve at time t in [0, 1]."
  [{:keys [points] :as curve} t]
  (when-not (<= 0 t 1)
    (throw (IllegalArgumentException. "Time t must be between 0 and 1.")))
  (let [basis (basis-function curve t)]
    [(reduce + (map * basis (map first points)))
     (reduce + (map * basis (map second points)))]))

(defn derivative
  "Tangent vector [dx dy] of the curve at time t in [0, 1]."
  [{:keys [points degree]} t]
  (when-not (<= 0 t 1)
    (throw (IllegalArgumentException. "Time t must be between 0 and 1.")))
  (reduce (fn [[dx dy] i]
            (let [coeff (* (binomial (dec degree) i)
                           (Math/pow (- 1 t) (- degree 1 i))
                           (Math/pow t i))]
              [(+ dx (* coeff (- (first (nth points (inc i)))
                                 (first (nth points i))) degree))
               (+ dy (* coeff (- (second (nth points (inc i)))
                                 (second (nth points i))) degree))]))
          [0.0 0.0] (range degree)))
