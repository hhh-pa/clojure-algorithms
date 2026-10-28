(ns clojure-algorithms.conversions.prefix-conversions
  "Port of conversions/prefix_conversions.py.
  Converts between SI and binary unit prefixes."
  (:require [clojure.string :as str]))

(def si-unit
  "SI prefix name to power-of-ten exponent."
  {"yotta" 24 "zetta" 21 "exa" 18 "peta" 15 "tera" 12 "giga" 9 "mega" 6
   "kilo" 3 "hecto" 2 "deca" 1 "deci" -1 "centi" -2 "milli" -3 "micro" -6
   "nano" -9 "pico" -12 "femto" -15 "atto" -18 "zepto" -21 "yocto" -24})

(def binary-unit
  "Binary prefix name to power-of-two exponent factor."
  {"yotta" 8 "zetta" 7 "exa" 6 "peta" 5 "tera" 4 "giga" 3 "mega" 2 "kilo" 1})

(defn- resolve-prefix [table prefix]
  (if (string? prefix)
    (get table (str/lower-case prefix))
    (get table (str/lower-case (name prefix)))))

(defn convert-si-prefix
  "Convert KNOWN-AMOUNT from KNOWN-PREFIX to UNKNOWN-PREFIX (SI)."
  [known-amount known-prefix unknown-prefix]
  (* known-amount (Math/pow 10 (- (resolve-prefix si-unit known-prefix)
                                  (resolve-prefix si-unit unknown-prefix)))))

(defn convert-binary-prefix
  "Convert KNOWN-AMOUNT from KNOWN-PREFIX to UNKNOWN-PREFIX (binary)."
  [known-amount known-prefix unknown-prefix]
  (* known-amount (Math/pow 2 (* (- (resolve-prefix binary-unit known-prefix)
                                    (resolve-prefix binary-unit unknown-prefix))
                                 10))))
