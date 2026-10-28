(ns clojure-algorithms.conversions.decimal-to-binary
  "Port of conversions/decimal_to_binary.py.
  Converts decimal integers to binary strings."
  (:require [clojure.string :as str]))

(defn decimal-to-binary-iterative
  "Convert integer NUM to a \"0b\"-prefixed binary string."
  [num]
  (when (float? num)
    (throw (ex-info "'float' object cannot be interpreted as an integer"
                    {:type :type-error})))
  (when (string? num)
    (throw (ex-info "'str' object cannot be interpreted as an integer"
                    {:type :type-error})))
  (if (zero? num)
    "0b0"
    (let [neg? (neg? num)
          bits (loop [n (if neg? (- num) num) acc ""]
                 (if (zero? n) acc
                     (recur (bit-shift-right n 1) (str (rem n 2) acc))))]
      (str (if neg? "-0b" "0b") bits))))

(defn decimal-to-binary-recursive-helper
  "Binary digits of a positive integer (accepts numeric strings)."
  [decimal]
  (let [n (try (Long/parseLong (str/trim (str decimal)))
               (catch NumberFormatException _
                 (throw (ex-info (str "invalid literal for int() with base 10: '"
                                      decimal "'")
                                 {:type :value-error}))))]
    (if (#{0 1} n)
      (str n)
      (let [[d m] [(quot n 2) (rem n 2)]]
        (str (decimal-to-binary-recursive-helper d) m)))))

(defn decimal-to-binary-recursive
  "Convert integer NUMBER to a \"0b\"/\"-0b\"-prefixed binary string."
  [number]
  (let [s (str/trim (str number))]
    (when (empty? s)
      (throw (ex-info "No input value was provided" {:type :value-error})))
    (let [neg? (str/starts-with? s "-")
          body (str/replace s #"^-+" "")]
      (when-not (re-matches #"\d+" (or body ""))
        (throw (ex-info "Input value is not an integer" {:type :value-error})))
      (str (if neg? "-" "") "0b"
           (decimal-to-binary-recursive-helper (Long/parseLong body))))))
