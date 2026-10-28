(ns clojure-algorithms.conversions.binary-to-decimal
  "Port of conversions/binary_to_decimal.py."
  (:require [clojure.string :as str]))

(defn bin-to-decimal
  "Convert binary digit string BIN-STRING to a decimal integer."
  [bin-string]
  (let [s (str/trim (str bin-string))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)]
      (when-not (every? #{\0 \1} body)
        (throw (ex-info "Non-binary value was passed to the function"
                        {:type :value-error})))
      (let [n (reduce (fn [acc c] (+ (* acc 2) (- (int c) (int \0)))) 0 body)]
        (if neg? (- n) n)))))
