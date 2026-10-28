(ns clojure-algorithms.conversions.hexadecimal-to-decimal
  "Port of conversions/hexadecimal_to_decimal.py."
  (:require [clojure.string :as str]))

(def ^:private hex-table
  (into {} (map-indexed (fn [i c] [c i])) "0123456789abcdef"))

(defn hex-to-decimal
  "Convert hex string HEX-STRING to a decimal integer."
  [hex-string]
  (let [s (str/lower-case (str/trim hex-string))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)]
      (when-not (every? #(contains? hex-table %) body)
        (throw (ex-info "Non-hexadecimal value was passed to the function"
                        {:type :value-error})))
      (let [n (reduce (fn [acc c] (+ (* acc 16) (hex-table c))) 0 body)]
        (if neg? (- n) n)))))
