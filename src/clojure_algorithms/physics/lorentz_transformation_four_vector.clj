(ns clojure-algorithms.physics.lorentz-transformation-four-vector
  "Lorentz boosts in the x direction as linear transforms of four-vectors.

  Pure-Clojure port of the numpy/sympy reference: matrices and four-vectors
  are nested vectors of doubles. The symbolic (sympy) mode has no stdlib
  equivalent, so transform requires a numeric [t x y z] event vector.
  Reference: https://en.wikipedia.org/wiki/Lorentz_transformation")

(def c
  "Speed of light in m/s."
  299792458.0)

(defn beta
  "Returns beta = v/c for a velocity in m/s."
  [velocity]
  (cond
    (> velocity c)
    (throw (ex-info "Speed must not exceed light speed 299,792,458 [m/s]!"
                    {:velocity velocity}))
    (< velocity 1)
    (throw (ex-info "Speed must be greater than or equal to 1!"
                    {:velocity velocity}))
    :else (/ velocity c)))

(defn lorentz-gamma
  "Returns the Lorentz factor 1/sqrt(1-v^2/c^2) for a velocity in m/s."
  [velocity]
  (/ 1 (Math/sqrt (- 1 (Math/pow (beta velocity) 2)))))

(defn transformation-matrix
  "Returns the 4x4 Lorentz boost matrix (nested vectors) for a velocity in m/s."
  [velocity]
  (let [g (lorentz-gamma velocity)
        gb (* g (beta velocity))]
    [[g (- gb) 0 0]
     [(- gb) g 0 0]
     [0 0 1 0]
     [0 0 0 1]]))

(defn transform
  "Applies the Lorentz boost for velocity to the numeric event [t x y z].
  The time entry is multiplied by c (four-vector [ct x y z]) before the
  matrix multiplication. Returns the transformed four-vector."
  [velocity event]
  (let [m (transformation-matrix velocity)
        [t x y z] event
        v [(* c t) x y z]]
    (mapv (fn [row] (reduce + (map * row v))) m)))
