(ns clojure-algorithms.conversions.base64-to-binary
  "Port of conversions/base64_to_binary.py.
  Converts a base64 string to its binary digit string."
  (:require [clojure.string :as str]))

(def ^:private alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(def b64-to-bits
  "Map of base64 character to its 6-bit binary string."
  (into {}
        (map-indexed (fn [i c]
                       (let [s (Integer/toBinaryString i)]
                         [c (str (apply str (repeat (- 6 (count s)) \0)) s)])))
        alphabet))

(defn base64-to-bin
  "Convert base64 string B64-STR to a binary digit string.
  A leading '-' is preserved as a sign."
  [base64-str]
  (let [s (str/trim (str base64-str))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)
          bits (apply str
                      (map-indexed
                       (fn [i ch]
                         (if-let [b (get b64-to-bits ch)]
                           b
                           (throw (ex-info
                                   (str "Invalid base64 string. Invalid char "
                                        ch " at pos " i)
                                   {:type :value-error}))))
                       body))]
      (if neg? (str "-" bits) bits))))
