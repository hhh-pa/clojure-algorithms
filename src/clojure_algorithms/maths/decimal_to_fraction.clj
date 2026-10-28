(ns clojure-algorithms.maths.decimal-to-fraction
  "Convert a decimal number to its simplest fraction form."
  (:require [clojure.string :as str]))

(defn decimal-to-fraction
  "Return [numerator denominator] for the given decimal (number or string)."
  [decimal]
  (let [d (try
            (cond (number? decimal) (double decimal)
                  (string? decimal) (Double/parseDouble decimal)
                  :else (throw (ex-info "Please enter a valid number"
                                        {:type :ValueError})))
            (catch NumberFormatException _
              (throw (ex-info "Please enter a valid number" {:type :ValueError}))))
        int-part (long d)]
    (if (zero? (- d int-part))
      [int-part 1]
      (let [digits (count (second (str/split (str d) #"\.")))
            scale (long (Math/pow 10 digits))
            numerator (long (* d scale))
            denominator scale
            divisor (loop [divisor denominator
                           dividend numerator]
                      (let [remainder (mod dividend divisor)]
                        (if (zero? remainder)
                          divisor
                          (recur remainder divisor))))]
        [(quot numerator divisor) (quot denominator divisor)]))))
