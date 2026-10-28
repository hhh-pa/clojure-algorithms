(ns clojure-algorithms.conversions.ipv4-conversion
  "Port of conversions/ipv4_conversion.py.
  Converts between IPv4 dotted-quad strings and decimal integers."
  (:require [clojure.string :as str]))

(defn ipv4-to-decimal
  "Convert IPv4 dotted-quad string to its decimal representation."
  [ipv4-address]
  (let [parts (str/split ipv4-address #"\.")
        octets (try (mapv #(Integer/parseInt %) parts)
                    (catch NumberFormatException _
                      (throw (ex-info "Invalid IPv4 address format"
                                      {:type :value-error}))))]
    (when-not (= 4 (count octets))
      (throw (ex-info "Invalid IPv4 address format" {:type :value-error})))
    (reduce (fn [acc o]
              (when-not (<= 0 o 255)
                (throw (ex-info (str "Invalid IPv4 octet " o)
                                {:type :value-error})))
              (+ (bit-shift-left acc 8) o))
            0 octets)))

(defn alt-ipv4-to-decimal
  "Convert IPv4 dotted-quad string to decimal via hex concatenation."
  [ipv4-address]
  (Long/parseLong (apply str "0"
                         (map #(format "%02x" (Integer/parseInt %))
                              (str/split ipv4-address #"\.")))
                  16))

(defn decimal-to-ipv4
  "Convert decimal integer to its IPv4 dotted-quad string."
  [decimal-ipv4]
  (when-not (<= 0 decimal-ipv4 4294967295)
    (throw (ex-info "Invalid decimal IPv4 address" {:type :value-error})))
  (str/join "." (map #(bit-and (bit-shift-right decimal-ipv4 %) 255)
                     [24 16 8 0])))
