(ns clojure-algorithms.physics.first-law-of-thermodynamics
  "First law of thermodynamics: Q = dU + W.

  Heat added, internal energy variation and work must all use equal units.
  https://en.wikipedia.org/wiki/Laws_of_thermodynamics")

(defn- check-arg
  "Throws ex-info unless the argument is a number."
  [argument]
  (when (not (number? argument))
    (throw (ex-info "Invalid argument. Should be an integer or float."
                    {:argument argument}))))

(defn categorize-system
  "Returns a string describing the system state for a value of the named
  quantity (:work, :heat or :internal-energy-variation)."
  [argument-value argument-name]
  (case argument-name
    :work (cond (zero? argument-value) "The system is isochoric (constant volume)."
                (pos? argument-value) "The system is expanding."
                :else "The system is compressing.")
    :heat (cond (zero? argument-value)
                "The system is adiabatic (no heat exchange)."
                (pos? argument-value)
                "The system is endothermic (absorbing heat)."
                :else "The system is exothermic (releasing heat).")
    :internal-energy-variation
    (cond (zero? argument-value)
          "The system is isothermic (constant internal energy)"
          (pos? argument-value)
          "The internal energy of the system is increasing. It heating up."
          :else
          "The internal energy of the system is decreasing. It cooling down.")
    (throw (ex-info "Should be 'work', 'heat', or 'internal_energy_variation'."
                    {:name argument-name}))))

(defn- round1 [x] (/ (double (Math/round (* x 10))) 10))

(defn work-done
  "Returns the work W = Q - dU, printing the system categorizations."
  [heat internal-energy-variation]
  (check-arg heat)
  (check-arg internal-energy-variation)
  (println (categorize-system heat :heat))
  (println (categorize-system internal-energy-variation :internal-energy-variation))
  (let [w (- heat internal-energy-variation)]
    (println (categorize-system w :work))
    (round1 w)))

(defn heat-added
  "Returns the heat Q = dU + W, printing the system categorizations."
  [internal-energy-variation work]
  (check-arg internal-energy-variation)
  (check-arg work)
  (println (categorize-system internal-energy-variation :internal-energy-variation))
  (println (categorize-system work :work))
  (let [q (round1 (+ internal-energy-variation work))]
    (println (categorize-system q :heat))
    q))

(defn internal-energy-variation
  "Returns dU = Q - W, printing the system categorizations."
  [heat work]
  (check-arg heat)
  (check-arg work)
  (println (categorize-system heat :heat))
  (println (categorize-system work :work))
  (let [u (round1 (- heat work))]
    (println (categorize-system u :internal-energy-variation))
    u))
