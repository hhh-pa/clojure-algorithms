(ns clojure-algorithms.physics.hubble-parameter
  "Hubble parameter H(z), the Universe expansion rate at a given redshift.

  H = H0 * sqrt(radiation*(z+1)^4 + matter*(z+1)^3 + curvature*(z+1)^2 + dark).")

(defn hubble-parameter
  "Returns the Hubble parameter given the Hubble constant, relative radiation,
  matter and dark-energy densities and the redshift."
  [hubble-constant radiation-density matter-density dark-energy redshift]
  (let [params [redshift radiation-density matter-density dark-energy]]
    (when (some neg? params)
      (throw (ex-info "All input parameters must be positive" {})))
    (when (some #(> % 1) [radiation-density matter-density dark-energy])
      (throw (ex-info "Relative densities cannot be greater than one" {}))))
  (let [curvature (- 1 (+ matter-density radiation-density dark-energy))
        e2 (+ (* radiation-density (Math/pow (+ redshift 1) 4))
              (* matter-density (Math/pow (+ redshift 1) 3))
              (* curvature (Math/pow (+ redshift 1) 2))
              dark-energy)]
    (* hubble-constant (Math/pow e2 (/ 1 2)))))
