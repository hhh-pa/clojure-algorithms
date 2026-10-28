(ns clojure-algorithms.strings.credit-card-validator
  "Credit card number validation (Luhn algorithm).
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn validate-initial-digits
  "True when `number` starts with a known issuer prefix."
  [number]
  (boolean (some #(str/starts-with? number %) ["34" "35" "37" "4" "5" "6"])))

(defn luhn-validation
  "True when digit string `number` passes the Luhn check."
  [number]
  (let [digits (mapv #(Character/digit ^char % 10) number)
        n (count digits)
        total (reduce + (map-indexed
                         (fn [i d]
                           (if (odd? (- (dec n) i))
                             (let [doubled (* 2 d)]
                               (if (> doubled 9)
                                 (inc (mod doubled 10))
                                 doubled))
                             d))
                         digits))]
    (zero? (mod total 10))))

(defn validate-credit-card-number
  "Validates `number`, printing the reason. Returns true/false."
  [number]
  (let [err (str number " is an invalid credit card number because")]
    (cond
      (not (re-matches #"\d+" number))
      (do (println err "it has nonnumerical characters.") false)

      (not (<= 13 (count number) 16))
      (do (println err "of its length.") false)

      (not (validate-initial-digits number))
      (do (println err "of its first two digits.") false)

      (not (luhn-validation number))
      (do (println err "it fails the Luhn check.") false)

      :else
      (do (println number "is a valid credit card number.") true))))
