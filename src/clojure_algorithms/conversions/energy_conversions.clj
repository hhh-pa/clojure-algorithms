(ns clojure-algorithms.conversions.energy-conversions
  "Port of conversions/energy_conversions.py.
  Converts between energy units."
  (:require [clojure.string :as str]))

(def energy-conversion-table
  "Energy unit name to joules per unit."
  {"joule" 1.0
   "kilojoule" 1000
   "megajoule" 1000000
   "gigajoule" 1000000000
   "wattsecond" 1.0
   "watthour" 3600
   "kilowatthour" 3600000
   "newtonmeter" 1.0
   "calorie_nutr" 4186.8
   "kilocalorie_nutr" 4186800.00
   "electronvolt" 1.602176634e-19
   "britishthermalunit_it" 1055.05585
   "footpound" 1.355818})

(defn energy-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE."
  [from-type to-type value]
  (when-not (and (contains? energy-conversion-table to-type)
                 (contains? energy-conversion-table from-type))
    (throw (ex-info (str "Incorrect 'from_type' or 'to_type' value: '"
                         from-type "', '" to-type "'\nValid values are: "
                         (str/join ", " (keys energy-conversion-table)))
                    {:type :value-error})))
  (when-not (number? value)
    (throw (ex-info (str "unsupported operand type(s) for /: '"
                         (type value) "' and 'float'")
                    {:type :type-error})))
  (double (/ (* (double value) (energy-conversion-table from-type))
             (energy-conversion-table to-type))))
