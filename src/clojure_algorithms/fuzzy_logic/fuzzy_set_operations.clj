(ns clojure-algorithms.fuzzy-logic.fuzzy-set-operations)

;; Zadeh's fuzzy-set operators on membership vectors (plain Clojure
;; vectors stand in for the original NumPy arrays).

(defn triangular-membership
  "Sample a triangular membership function (left/peak/right) on grid.
  Returns a vector of degrees in [0, 1]."
  [grid left peak right]
  (when (not (<= left peak right))
    (throw (ex-info (str "Expected left <= peak <= right, got "
                         left ", " peak ", " right) {})))
  (mapv (fn [x]
          (let [left-slope (cond
                             (> peak left) (/ (- x left) (- peak left))
                             (< x peak) 0.0
                             :else 1.0)
                right-slope (cond
                              (> right peak) (/ (- right x) (- right peak))
                              (> x peak) 0.0
                              :else 1.0)]
            (min 1.0 (max 0.0 (min left-slope right-slope)))))
        grid))

(defn fuzzy-union
  "Union (logical OR): max(mu-A, mu-B) element-wise."
  [a b]
  (mapv max a b))

(defn fuzzy-intersection
  "Intersection (logical AND): min(mu-A, mu-B) element-wise."
  [a b]
  (mapv min a b))

(defn fuzzy-complement
  "Complement (logical NOT): 1 - mu element-wise."
  [membership]
  (mapv #(- 1.0 %) membership))

(defn fuzzy-difference
  "Difference A/B: min(mu-A, 1 - mu-B) element-wise."
  [a b]
  (mapv (fn [x y] (min x (- 1.0 y))) a b))

(defn algebraic-sum
  "Algebraic (probabilistic) sum: a + b - a*b element-wise."
  [a b]
  (mapv (fn [x y] (+ x y (- (* x y)))) a b))

(defn algebraic-product
  "Algebraic product: a * b element-wise."
  [a b]
  (mapv * a b))

(defn bounded-sum
  "Bounded sum (Lukasiewicz t-conorm): min(1, a + b) element-wise."
  [a b]
  (mapv (fn [x y] (min 1.0 (+ x y))) a b))

(defn bounded-difference
  "Bounded difference (Lukasiewicz t-norm): max(0, a + b - 1) element-wise."
  [a b]
  (mapv (fn [x y] (max 0.0 (+ x y -1.0))) a b))
