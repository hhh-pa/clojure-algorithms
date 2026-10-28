(ns clojure-algorithms.computer-vision.harris-corner
  "Harris corner detection over 2D vectors (cv2 file IO omitted).")

(defn make-harris
  "Creates a detector; k must be 0.04 or 0.06 per the source."
  [k window-size]
  (when-not (contains? #{0.04 0.06} k)
    (throw (IllegalArgumentException. "invalid k value")))
  {:k k :window-size window-size})

(defn- grad-1d
  [v]
  (let [n (count v)]
    (mapv (fn [i]
            (cond (zero? i) (double (- (nth v 1) (nth v 0)))
                  (= i (dec n)) (double (- (nth v i) (nth v (dec i))))
                  :else (/ (double (- (nth v (inc i)) (nth v (dec i)))) 2.0)))
          (range n))))

(defn- gradients
  [img]
  (let [h (count img) w (count (first img))
        dx (mapv grad-1d img)
        cols (mapv (fn [c] (mapv #(nth % c) img)) (range w))
        dy-cols (mapv grad-1d cols)
        dy (mapv (fn [r] (mapv #(nth % r) dy-cols)) (range h))]
    [dy dx]))

(defn detect
  "Returns corner [x y response] triples of a grayscale matrix."
  [{:keys [window-size]} img]
  (let [[dy dx] (gradients img)
        h (count img) w (count (first img))
        ixx (mapv #(mapv (fn [v] (* v v)) %) dx)
        iyy (mapv #(mapv (fn [v] (* v v)) %) dy)
        ixy (mapv (fn [a b] (mapv * a b)) dx dy)
        offset (quot window-size 2)
        k 0.04]
    (vec (for [y (range offset (- h offset))
               x (range offset (- w offset))
               :let [wxx (reduce + (for [yy (range (- y offset) (+ y offset 1))
                                         xx (range (- x offset) (+ x offset 1))]
                                     (get-in ixx [yy xx])))
                     wyy (reduce + (for [yy (range (- y offset) (+ y offset 1))
                                         xx (range (- x offset) (+ x offset 1))]
                                     (get-in iyy [yy xx])))
                     wxy (reduce + (for [yy (range (- y offset) (+ y offset 1))
                                         xx (range (- x offset) (+ x offset 1))]
                                     (get-in ixy [yy xx])))
                     det (- (* wxx wyy) (* wxy wxy))
                     trace (+ wxx wyy)
                     r (- det (* k trace trace))]
               :when (> r 0.5)]
           [x y r]))))
