(ns clojure-algorithms.conversions.length-conversion
  "Port of conversions/length_conversion.py.
  Converts between length units."
  (:require [clojure.string :as str]))

(def type-conversion
  "Unit name to abbreviation."
  {"millimeter" "mm" "centimeter" "cm" "meter" "m" "kilometer" "km"
   "inch" "in" "inche" "in" "feet" "ft" "foot" "ft" "yard" "yd" "mile" "mi"})

(def metric-conversion
  "Abbreviation to [from-factor to-factor] relative to a metre."
  {"mm" [0.001 1000] "cm" [0.01 100] "m" [1 1] "km" [1000 0.001]
   "in" [0.0254 39.3701] "ft" [0.3048 3.28084] "yd" [0.9144 1.09361]
   "mi" [1609.34 0.000621371]})

(defn length-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE."
  [value from-type to-type]
  (let [new-from (get type-conversion
                      (str/replace (str/lower-case from-type) #"s+$" "")
                      (str/replace (str/lower-case from-type) #"s+$" ""))
        new-to (get type-conversion
                    (str/replace (str/lower-case to-type) #"s+$" "")
                    (str/replace (str/lower-case to-type) #"s+$" ""))]
    (when-not (contains? metric-conversion new-from)
      (throw (ex-info (str "Invalid 'from_type' value: '" from-type "'.\n"
                           "Conversion abbreviations are: "
                           (str/join ", " (keys metric-conversion)))
                      {:type :value-error})))
    (when-not (contains? metric-conversion new-to)
      (throw (ex-info (str "Invalid 'to_type' value: '" to-type "'.\n"
                           "Conversion abbreviations are: "
                           (str/join ", " (keys metric-conversion)))
                      {:type :value-error})))
    (* value (first (metric-conversion new-from))
       (second (metric-conversion new-to)))))
