(ns clojure-algorithms.conversions.binary-to-gray-code
  "Port of conversions/binary_to_gray_code.py.
  Conversions between binary integers and Gray code.")

(defn- check-nonneg-int [n]
  (when (or (boolean? n) (not (integer? n)))
    (throw (ex-info "Input must be an integer" {:type :type-error})))
  (when (neg? n)
    (throw (ex-info "Input must be a non-negative integer"
                    {:type :value-error}))))

(defn binary-to-gray
  "Convert non-negative integer BINARY-NUMBER to its Gray code integer."
  [binary-number]
  (check-nonneg-int binary-number)
  (bit-xor binary-number (bit-shift-right binary-number 1)))

(defn gray-to-binary
  "Convert Gray code integer GRAY-NUMBER to its binary integer."
  [gray-number]
  (check-nonneg-int gray-number)
  (loop [b gray-number g (bit-shift-right gray-number 1)]
    (if (zero? g) b
        (recur (bit-xor b g) (bit-shift-right g 1)))))

(defn decimal-to-gray
  "Convert non-negative integer DECIMAL-NUMBER to a Gray code binary string."
  [decimal-number]
  (check-nonneg-int decimal-number)
  (Long/toBinaryString (long (binary-to-gray decimal-number))))

(defn gray-to-decimal
  "Convert Gray code binary string GRAY-STRING to its decimal integer."
  [gray-string]
  (when (empty? gray-string)
    (throw (ex-info "Input string cannot be empty" {:type :value-error})))
  (when-not (every? #{\0 \1} gray-string)
    (throw (ex-info "Invalid binary string" {:type :value-error})))
  (gray-to-binary (Long/parseLong gray-string 2)))
