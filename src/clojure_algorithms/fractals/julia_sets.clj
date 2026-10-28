(ns clojure-algorithms.fractals.julia-sets)

;; Pure-logic port of the NumPy/matplotlib original.
;; Complex numbers are [re im] vectors; grids are vectors of rows.
;; Matplotlib display (show-results) is intentionally not ported:
;; use inside-escape-radius? on the iterated grid instead.

(def c-cauliflower [0.25 0.0])
(def c-polynomial-1 [-0.4 0.6])
(def c-polynomial-2 [-0.1 0.651])
(def c-exponential [-2.0 0.0])

(defn c-add [[a b] [c d]] [(+ a c) (+ b d)])

(defn c-mul [[a b] [c d]]
  [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn c-abs [[a b]] (Math/sqrt (+ (* a a) (* b b))))

(defn c-exp
  "Complex exponential e^z."
  [[a b]]
  (let [r (Math/exp a)]
    [(* r (Math/cos b)) (* r (Math/sin b))]))

(defn eval-exponential
  "Evaluate e^z + c."
  [c-param z]
  (c-add (c-exp z) c-param))

(defn eval-quadratic-polynomial
  "Evaluate z^2 + c."
  [c-param z]
  (c-add (c-mul z z) c-param))

(defn prepare-grid
  "Grid of complex values with re/im ranging from -window-size to
  window-size over nb-pixels points per axis. Returns a vector of rows."
  [window-size nb-pixels]
  (let [axis (if (= nb-pixels 1)
               [0.0]
               (let [step (/ (* 2.0 window-size) (dec nb-pixels))]
                 (mapv #(+ (- window-size) (* % step)) (range nb-pixels))))]
    (mapv (fn [x] (mapv (fn [y] [x y]) axis)) axis)))

(defn iterate-function
  "Iterate eval-fn nb-iterations times starting from grid z-0.
  Non-finite values are replaced by infinity when given."
  ([eval-fn fn-params nb-iterations z-0]
   (iterate-function eval-fn fn-params nb-iterations z-0 nil))
  ([eval-fn fn-params nb-iterations z-0 infinity]
   (let [sanitize (fn [[re im :as z]]
                    (if (and (some? infinity)
                             (or (Double/isNaN re) (Double/isNaN im)
                                 (Double/isInfinite re) (Double/isInfinite im)))
                      [infinity infinity]
                      z))]
     (nth (iterate (fn [grid]
                     (mapv (fn [row] (mapv #(sanitize (eval-fn fn-params %)) row))
                           grid))
                   z-0)
          nb-iterations))))

(defn inside-escape-radius?
  "True when the final iterate z is inside the escape radius, i.e. its
  orbit is treated as bounded (part of the filled Julia set)."
  [z escape-radius]
  (< (c-abs z) escape-radius))
