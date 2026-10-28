(ns clojure-algorithms.strings.is-spain-national-id
  "Validation of the Spanish DNI national id.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def numbers-plus-letter "Input must be a string of 8 numbers plus letter")
(def lookup-letters "TRWAGMYFPDXBNJZSQVHLCKE")

(defn is-spain-national-id
  "True when `spanish-id` (8 digits + check letter) is a valid DNI.
  Throws on malformed input."
  [spanish-id]
  (when-not (string? spanish-id)
    (throw (IllegalArgumentException.
            (str "Expected string as input, found "
                 (.getSimpleName ^Class (type spanish-id))))))
  (let [clean (str/upper-case (str/replace spanish-id "-" ""))]
    (when-not (= 9 (count clean))
      (throw (IllegalArgumentException. numbers-plus-letter)))
    (let [digits (subs clean 0 8)
          letter (nth clean 8)
          number (try (Long/parseLong digits)
                      (catch NumberFormatException _
                        (throw (IllegalArgumentException.
                                numbers-plus-letter))))]
      (when (Character/isDigit ^char letter)
        (throw (IllegalArgumentException. numbers-plus-letter)))
      (= letter (nth lookup-letters (mod number 23))))))
