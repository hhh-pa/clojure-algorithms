(ns clojure-algorithms.conversions.weight-conversion
  "Port of conversions/weight_conversion.py.
  Converts between weight units."
  (:require [clojure.string :as str]))

(def kilogram-chart
  "Weight unit to units per kilogram."
  {"kilogram" 1 "gram" 1000 "milligram" 1000000 "metric-ton" 0.001
   "long-ton" 0.0009842073 "short-ton" 0.0011023122 "pound" 2.2046244202
   "stone" 0.1574731728 "ounce" 35.273990723 "carrat" 5000
   "atomic-mass-unit" 6.022136652e26})

(def weight-type-chart
  "Weight unit to kilograms per unit."
  {"kilogram" 1 "gram" 0.001 "milligram" 1e-06 "metric-ton" 1000
   "long-ton" 1016.04608 "short-ton" 907.184 "pound" 0.453592
   "stone" 6.35029 "ounce" 0.0283495 "carrat" 0.0002
   "atomic-mass-unit" 1.660540199e-27})

(defn weight-conversion
  "Convert VALUE from FROM-TYPE to TO-TYPE."
  [from-type to-type value]
  (when-not (and (contains? weight-type-chart from-type)
                 (contains? kilogram-chart to-type))
    (throw (ex-info (str "Incorrect 'from_type' or 'to_type' value: '"
                         from-type "', '" to-type "'\nValid values are: "
                         (str/join ", " (keys weight-type-chart)))
                    {:type :value-error})))
  (* value (kilogram-chart to-type) (weight-type-chart from-type)))
