(ns clojure-algorithms.conversions.octal-to-decimal
  "Port of conversions/octal_to_decimal.py."
  (:require [clojure.string :as str]))

(defn oct-to-decimal
  "Convert octal string OCT-STRING to a decimal integer."
  [oct-string]
  (let [s (str/trim (str oct-string))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)]
      (when-not (and (seq body)
                     (every? #(<= (int \0) (int %) (int \7)) body))
        (throw (ex-info "Non-octal value was passed to the function"
                        {:type :value-error})))
      (let [n (reduce (fn [acc c] (+ (* acc 8) (- (int c) (int \0))))
                      0 body)]
        (if neg? (- n) n)))))
