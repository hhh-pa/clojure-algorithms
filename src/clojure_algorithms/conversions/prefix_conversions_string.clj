(ns clojure-algorithms.conversions.prefix-conversions-string
  "Port of conversions/prefix_conversions_string.py.
  Formats a number with its best-fit SI or binary prefix.")

(def si-positive
  "SI prefixes with positive exponents, largest first."
  [["yotta" 24] ["zetta" 21] ["exa" 18] ["peta" 15] ["tera" 12] ["giga" 9]
   ["mega" 6] ["kilo" 3] ["hecto" 2] ["deca" 1]])

(def si-negative
  "SI prefixes with negative exponents."
  [["deci" -1] ["centi" -2] ["milli" -3] ["micro" -6] ["nano" -9]
   ["pico" -12] ["femto" -15] ["atto" -18] ["zepto" -21] ["yocto" -24]])

(def binary-units
  "Binary prefixes, largest first."
  [["yotta" 80] ["zetta" 70] ["exa" 60] ["peta" 50] ["tera" 40] ["giga" 30]
   ["mega" 20] ["kilo" 10]])

(defn add-si-prefix
  "Format VALUE with the largest SI prefix whose quotient exceeds 1."
  [value]
  (let [prefixes (if (pos? value) si-positive si-negative)]
    (or (some (fn [[pname pexp]]
                (let [numerical (/ (double value) (Math/pow 10 pexp))]
                  (when (> numerical 1) (str numerical " " pname))))
              prefixes)
        (str value))))

(defn add-binary-prefix
  "Format VALUE with the largest binary prefix whose quotient exceeds 1."
  [value]
  (or (some (fn [[pname pexp]]
              (let [numerical (/ (double value) (Math/pow 2 pexp))]
                (when (> numerical 1) (str numerical " " pname))))
            binary-units)
      (str value)))
