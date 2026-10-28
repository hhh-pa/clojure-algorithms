(ns clojure-algorithms.conversions.binary-to-base64
  "Port of conversions/binary_to_base64.py.
  Converts a binary digit string to base64."
  (:require [clojure.string :as str]))

(def ^:private alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(def bits-to-b64
  "Map of 6-bit binary string to base64 character."
  (into {}
        (map-indexed (fn [i _]
                       (let [s (Integer/toBinaryString i)]
                         [(str (apply str (repeat (- 6 (count s)) \0)) s)
                          (nth alphabet i)])))
        (range 64)))

(defn bin-to-base64
  "Convert binary digit string BIN-STR to base64.
  The input is left-padded with zeros up to the next multiple of 6 bits."
  [bin-str]
  (let [s (str/trim (str bin-str))]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (let [neg? (= \- (first s))
          body (if neg? (subs s 1) s)]
      (when-not (every? #{\0 \1} body)
        (throw (ex-info "Non-binary value was passed to the function"
                        {:type :value-error})))
      (let [pad-len (- (* 6 (inc (quot (count body) 6))) (count body))
            padded (str (apply str (repeat pad-len \0)) body)
            out (apply str (map #(get bits-to-b64 (apply str %))
                                (partition 6 padded)))]
        (if neg? (str "-" out) out)))))
