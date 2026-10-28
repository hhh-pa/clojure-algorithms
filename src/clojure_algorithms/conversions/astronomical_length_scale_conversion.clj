(ns clojure-algorithms.conversions.astronomical-length-scale-conversion
  "Port of conversions/astronomical_length_scale_conversion.py.
  Converts between astronomical length units via powers of ten."
  (:require [clojure.string :as str]))

(def unit-symbol
  "Full unit name to abbreviation."
  {"meter" "m" "kilometer" "km" "megametre" "Mm" "gigametre" "Gm"
   "terametre" "Tm" "petametre" "Pm" "exametre" "Em" "zettametre" "Zm"
   "yottametre" "Ym"})

(def metric-conversion
  "Abbreviation to exponent of ten relative to a metre."
  {"m" 0 "km" 3 "Mm" 6 "Gm" 9 "Tm" 12 "Pm" 15 "Em" 18 "Zm" 21 "Ym" 24})

(defn- sanitize [unit]
  (let [s (str/replace (str/lower-case unit) #"^s+|s+$" "")]
    (get unit-symbol s s)))

(defn length-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE.
  >>> (length-conversion 1 \"meter\" \"kilometer\") => 0.001"
  [value from-type to-type]
  (let [from-s (sanitize from-type)
        to-s (sanitize to-type)]
    (when-not (contains? metric-conversion from-s)
      (throw (ex-info (str "Invalid 'from_type' value: '" from-type "'.\n"
                           "Conversion abbreviations are: "
                           (str/join ", " (keys metric-conversion)))
                      {:type :value-error})))
    (when-not (contains? metric-conversion to-s)
      (throw (ex-info (str "Invalid 'to_type' value: '" to-type "'.\n"
                           "Conversion abbreviations are: "
                           (str/join ", " (keys metric-conversion)))
                      {:type :value-error})))
    (* value (Math/pow 10 (- (metric-conversion from-s)
                             (metric-conversion to-s))))))
