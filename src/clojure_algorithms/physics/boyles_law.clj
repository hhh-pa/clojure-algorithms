(ns clojure-algorithms.physics.boyles-law
  "Boyle's law: for a confined gas at constant temperature P1*V1 = P2*V2.

  Variables are passed as a map with keyword keys :p1 :v1 :p2 :v2 holding
  exactly three known positive values; the missing variable is computed.")

(def valid-variables
  "Variables recognised by Boyle's law solver."
  [:v1 :v2 :p1 :p2])

(defn check-validity
  "Validates a map of known values. Throws ex-info when invalid."
  [values]
  (when (not= (count values) 3)
    (throw (ex-info (format "Invalid input expected %d items, got %d"
                            3 (count values))
                    {:values values})))
  (doseq [[k v] values]
    (when (not (some #{k} valid-variables))
      (throw (ex-info (format "Invalid input %s is not a valid variable" (name k))
                      {:key k})))
    (when (<= v 0)
      (throw (ex-info (format "Invalid input %s must be greater than 0" (name k))
                      {:key k :value v}))))
  nil)

(defn find-target-variable
  "Returns the keyword of the missing variable to solve for."
  [values]
  (check-validity values)
  (or (first (remove #(contains? values %) valid-variables))
      (throw (ex-info "Input is invalid" {:values values}))))

(defn boyles-law
  "Solves for the missing pressure/volume variable.
  Returns a single-entry map of the target variable to a formatted string
  with units (\"Pa\" for pressure, \"L\" for volume)."
  [values]
  (check-validity values)
  (let [target (find-target-variable values)
        f (fn [x] (Double/parseDouble (format "%.3f" (double x))))]
    (case target
      :p1 {:p1 (str (f (/ (* (:p2 values) (:v2 values)) (:v1 values))) " Pa")}
      :v1 {:v1 (str (f (/ (* (:p2 values) (:v2 values)) (:p1 values))) " L")}
      :p2 {:p2 (str (f (/ (* (:p1 values) (:v1 values)) (:v2 values))) " Pa")}
      {:v2 (str (f (/ (* (:p1 values) (:v1 values)) (:p2 values))) " L")})))
