(ns clojure-algorithms.conversions.hex-to-bin
  "Port of conversions/hex_to_bin.py.
  Converts a hex string to the binary digits read back as an integer."
  (:require [clojure.string :as str]))

(defn hex-to-bin
  "Convert hex string HEX-NUM to binary digits returned as an integer,
  e.g. \"AC\" => 10101100."
  [hex-num]
  (let [s (str/trim hex-num)]
    (when (empty? s)
      (throw (ex-info "No value was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)
          n (try (Long/parseLong body 16)
                 (catch NumberFormatException _
                   (throw (ex-info "Invalid value was passed to the function"
                                   {:type :value-error}))))]
      (if (zero? n)
        0
        (let [digits (loop [x n acc ""]
                       (if (zero? x) acc
                           (recur (bit-shift-right x 1) (str (rem x 2) acc))))
              v (Long/parseLong digits)]
          (if neg? (- v) v))))))
