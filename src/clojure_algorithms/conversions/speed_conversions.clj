(ns clojure-algorithms.conversions.speed-conversions
  "Port of conversions/speed_conversions.py.
  Converts between speed units."
  (:require [clojure.string :as str]))

(def speed-chart
  "Speed unit to km/h per unit."
  {"km/h" 1.0 "m/s" 3.6 "mph" 1.609344 "knot" 1.852})

(def speed-chart-inverse
  "Speed unit to units per km/h."
  {"km/h" 1.0 "m/s" 0.277777778 "mph" 0.621371192 "knot" 0.539956803})

(defn convert-speed
  "Convert SPEED from UNIT-FROM to UNIT-TO, rounded to 3 decimals."
  [speed unit-from unit-to]
  (when-not (and (contains? speed-chart unit-to)
                 (contains? speed-chart-inverse unit-from))
    (throw (ex-info (str "Incorrect 'from_type' or 'to_type' value: '"
                         unit-from "', '" unit-to "'\nValid values are: "
                         (str/join ", " (keys speed-chart-inverse)))
                    {:type :value-error})))
  (/ (Math/rint (* speed (speed-chart unit-from)
                   (speed-chart-inverse unit-to) 1000.0))
     1000.0))
