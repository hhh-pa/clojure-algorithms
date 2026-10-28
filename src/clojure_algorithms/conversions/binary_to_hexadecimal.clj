(ns clojure-algorithms.conversions.binary-to-hexadecimal
  "Port of conversions/binary_to_hexadecimal.py.
  Converts a binary string to hexadecimal via the grouping method."
  (:require [clojure.string :as str]))

(def bits-to-hex
  "Map of 4-bit binary string to hex digit."
  {"0000" "0" "0001" "1" "0010" "2" "0011" "3"
   "0100" "4" "0101" "5" "0110" "6" "0111" "7"
   "1000" "8" "1001" "9" "1010" "a" "1011" "b"
   "1100" "c" "1101" "d" "1110" "e" "1111" "f"})

(defn bin-to-hexadecimal
  "Convert binary string BINARY-STR to a \"0x\"-prefixed hex string."
  [binary-str]
  (let [s (str/trim (str binary-str))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)]
      (when-not (every? #{\0 \1} body)
        (throw (ex-info "Non-binary value was passed to the function"
                        {:type :value-error})))
      (let [pad-len (- (* 4 (inc (quot (count body) 4))) (count body))
            padded (str (apply str (repeat pad-len \0)) body)
            out (str "0x" (apply str (map #(get bits-to-hex (apply str %))
                                          (partition 4 padded))))]
        (if neg? (str "-" out) out)))))
