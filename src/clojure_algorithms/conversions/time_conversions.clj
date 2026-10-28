(ns clojure-algorithms.conversions.time-conversions
  "Port of conversions/time_conversions.py.
  Converts between units of time."
  (:require [clojure.string :as str]))

(def time-chart
  "Time unit to seconds per unit."
  {"seconds" 1.0 "minutes" 60.0 "hours" 3600.0 "days" 86400.0
   "weeks" 604800.0 "months" 2629800.0 "years" 31557600.0})

(def time-chart-inverse
  "Time unit to units per second."
  (into {} (map (fn [[k v]] [k (/ 1.0 v)])) time-chart))

(defn convert-time
  "Convert TIME-VALUE from UNIT-FROM to UNIT-TO, rounded to 3 decimals."
  [time-value unit-from unit-to]
  (when-not (and (number? time-value) (>= time-value 0))
    (throw (ex-info "'time_value' must be a non-negative number."
                    {:type :value-error})))
  (let [uf (str/lower-case unit-from)
        ut (str/lower-case unit-to)]
    (when-not (contains? time-chart uf)
      (throw (ex-info (str "Invalid unit " uf " is not in "
                           (str/join ", " (keys time-chart)) ".")
                      {:type :value-error})))
    (when-not (contains? time-chart ut)
      (throw (ex-info (str "Invalid unit " ut " is not in "
                           (str/join ", " (keys time-chart)) ".")
                      {:type :value-error})))
    (/ (Math/rint (* time-value (time-chart uf)
                     (time-chart-inverse ut) 1000.0))
       1000.0)))
