(ns clojure-algorithms.physics.faraday-lenz-law
  "Faraday-Lenz law of induction: emf = -dPhi/dt.

  https://en.wikipedia.org/wiki/Faraday%27s_law_of_induction")

(defn- check-args
  "Validates argument types and time interval. Throws on invalid input."
  [final-flux initial-flux time-interval]
  (when (not (number? final-flux))
    (throw (ex-info "Invalid final flux. Should be an integer or float."
                    {:flux final-flux})))
  (when (not (number? initial-flux))
    (throw (ex-info "Invalid final flux. Should be an integer or float."
                    {:flux initial-flux})))
  (when (not (number? time-interval))
    (throw (ex-info "Invalid time interval. Should be an integer or float."
                    {:time time-interval})))
  (when (< time-interval 0)
    (throw (ex-info "Invalid time interval. Should be a positive number."
                    {:time time-interval}))))

(defn induced-electromotive-force
  "Returns the induced EMF in volts given final flux, initial flux (Wb)
  and time interval (s), rounded to 1 decimal."
  [final-flux initial-flux time-interval]
  (check-args final-flux initial-flux time-interval)
  (/ (double (Math/round (* (/ (- (- final-flux initial-flux)) time-interval) 10)))
     10))
