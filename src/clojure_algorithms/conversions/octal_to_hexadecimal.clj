(ns clojure-algorithms.conversions.octal-to-hexadecimal
  "Port of conversions/octal_to_hexadecimal.py.
  Converts an octal string to an uppercase \"0x\" hex string."
  (:require [clojure.string :as str]))

(def ^:private hex-chars "0123456789ABCDEF")

(defn octal-to-hex
  "Convert octal string OCTAL to an uppercase \"0x\"-prefixed hex string."
  [octal]
  (when-not (string? octal)
    (throw (ex-info "Expected a string as input" {:type :type-error})))
  (let [s (if (str/starts-with? octal "0o") (subs octal 2) octal)]
    (when (empty? s)
      (throw (ex-info "Empty string was passed to the function"
                      {:type :value-error})))
    (when-not (every? #(<= (int \0) (int %) (int \7)) s)
      (throw (ex-info "Not a Valid Octal Number" {:type :value-error})))
    (let [decimal (reduce (fn [acc c]
                            (bit-or (bit-shift-left acc 3)
                                    (- (int c) (int \0))))
                          0 s)
          revhex (loop [d decimal acc ""]
                   (if (zero? d) acc
                       (recur (bit-shift-right d 4)
                              (str acc (nth hex-chars (bit-and d 15))))))]
      (str "0x" (apply str (reverse revhex))))))
