(ns clojure-algorithms.physics.hamiltonian
  "Classical Hamiltonian H = T + V and the 1D quantum Hamiltonian matrix
  built with finite-difference discretization of the Schrodinger equation.

  H[i][i] = hbar^2/(m*dx^2) + V(xi), H[i][i+1] = H[i][i-1] = -hbar^2/(2*m*dx^2)")

(defn classical-hamiltonian
  "Returns H = 0.5*m*v^2 + V for mass, velocity and potential energy."
  [mass velocity potential-energy]
  (when (<= mass 0)
    (throw (ex-info "mass must be positive" {:mass mass})))
  (+ (* 0.5 mass velocity velocity) potential-energy))

(defn quantum-hamiltonian
  "Builds the num-points x num-points Hamiltonian matrix for a 1D potential
  (vector of length num-points). Optional keys :mass :hbar :dx default to 1.0."
  [num-points potential & {:keys [mass hbar dx]
                           :or {mass 1.0 hbar 1.0 dx 1.0}}]
  (when (<= num-points 0)
    (throw (ex-info "num_points must be positive" {:num-points num-points})))
  (when (not= (count potential) num-points)
    (throw (ex-info "potential must have length num_points" {})))
  (let [diag-term (/ (* hbar hbar) (* mass dx dx))
        off-diag-term (/ (- (* hbar hbar)) (* 2 mass dx dx))]
    (mapv (fn [i]
            (mapv (fn [j]
                    (cond (= i j) (+ diag-term (nth potential i))
                          (= (Math/abs (- i j)) 1) (double off-diag-term)
                          :else 0.0))
                  (range num-points)))
          (range num-points))))
