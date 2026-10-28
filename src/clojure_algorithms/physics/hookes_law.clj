(ns clojure-algorithms.physics.hookes-law
  "Hooke's law for spring restoring force: F = -k*x.

  https://en.wikipedia.org/wiki/Hooke%27s_law")

(defn hookes-law
  "Returns the restoring force in Newtons given spring constant (N/m,
  must be positive) and displacement from equilibrium (m)."
  [spring-constant displacement]
  (when (<= spring-constant 0)
    (throw (ex-info "Spring constant must be positive."
                    {:spring-constant spring-constant})))
  (* (- spring-constant) displacement))
