(ns clojure-algorithms.conversions.pressure-conversions
  "Port of conversions/pressure_conversions.py.
  Converts between pressure units."
  (:require [clojure.string :as str]))

(def pressure-conversion-table
  "Pressure unit to [from-factor to-factor] relative to atm."
  {"atm" [1 1]
   "pascal" [0.0000098 101325]
   "bar" [0.986923 1.01325]
   "kilopascal" [0.00986923 101.325]
   "megapascal" [9.86923 0.101325]
   "psi" [0.068046 14.6959]
   "inHg" [0.0334211 29.9213]
   "torr" [0.00131579 760]})

(defn pressure-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE."
  [value from-type to-type]
  (when-not (contains? pressure-conversion-table from-type)
    (throw (ex-info (str "Invalid 'from_type' value: '" from-type
                         "'  Supported values are:\n"
                         (str/join ", " (keys pressure-conversion-table)))
                    {:type :value-error})))
  (when-not (contains? pressure-conversion-table to-type)
    (throw (ex-info (str "Invalid 'to_type' value: '" to-type
                         "'.  Supported values are:\n"
                         (str/join ", " (keys pressure-conversion-table)))
                    {:type :value-error})))
  (* value (first (pressure-conversion-table from-type))
     (second (pressure-conversion-table to-type))))
