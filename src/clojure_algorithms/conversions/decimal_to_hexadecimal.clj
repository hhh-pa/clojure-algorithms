(ns clojure-algorithms.conversions.decimal-to-hexadecimal
  "Port of conversions/decimal_to_hexadecimal.py.
  Converts decimal integers to \"0x\"-prefixed hex strings.")

(def ^:private values
  {0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9"
   10 "a" 11 "b" 12 "c" 13 "d" 14 "e" 15 "f"})

(defn decimal-to-hexadecimal
  "Convert DECIMAL (int or integral float) to a \"0x\"-prefixed hex string."
  [decimal]
  (when-not (and (number? decimal) (== decimal (long decimal)))
    (throw (ex-info "AssertionError" {:type :assertion-error})))
  (let [n (long decimal)
        neg? (neg? n)
        body (loop [d (if neg? (- n) n) acc ""]
               (if (zero? d) acc
                   (recur (quot d 16) (str (values (rem d 16)) acc))))
        out (str "0x" body)]
    (if neg? (str "-" out) out)))
