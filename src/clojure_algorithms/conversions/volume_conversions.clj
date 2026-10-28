(ns clojure-algorithms.conversions.volume-conversions
  "Port of conversions/volume_conversions.py.
  Converts between volume units."
  (:require [clojure.string :as str]))

(def volume-conversion-table
  "Volume unit to [from-factor to-factor] relative to a cubic metre."
  {"cubic meter" [1 1]
   "litre" [0.001 1000]
   "kilolitre" [1 1]
   "gallon" [0.00454 264.172]
   "cubic yard" [0.76455 1.30795]
   "cubic foot" [0.028 35.3147]
   "cup" [0.000236588 4226.75]})

(defn volume-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE."
  [value from-type to-type]
  (when-not (contains? volume-conversion-table from-type)
    (throw (ex-info (str "Invalid 'from_type' value: '" from-type "'.\n"
                         "Supported values are: "
                         (str/join ", " (keys volume-conversion-table)))
                    {:type :value-error})))
  (when-not (contains? volume-conversion-table to-type)
    (throw (ex-info (str "Invalid 'to_type' value: '" to-type "'.\n"
                         "Supported values are: "
                         (str/join ", " (keys volume-conversion-table)))
                    {:type :value-error})))
  (* value (first (volume-conversion-table from-type))
     (second (volume-conversion-table to-type))))
