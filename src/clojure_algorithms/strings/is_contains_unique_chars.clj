(ns clojure-algorithms.strings.is-contains-unique-chars
  "Check that every character in a string is unique (bitmap approach).
  Ported from TheAlgorithms/Python."
  (:import [java.math BigInteger]))

(defn is-contains-unique-chars
  "True when all characters in `s` are distinct."
  [s]
  (loop [chars (seq s) ^BigInteger bitmap BigInteger/ZERO]
    (if (nil? chars)
      true
      (let [code (int (first chars))]
        (if (.testBit bitmap code)
          false
          (recur (next chars) (.setBit bitmap code)))))))
