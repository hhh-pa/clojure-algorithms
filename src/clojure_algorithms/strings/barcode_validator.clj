(ns clojure-algorithms.strings.barcode-validator
  "EAN-13 barcode check-digit computation and validation.
  See https://en.wikipedia.org/wiki/Check_digit#Algorithms
  Ported from TheAlgorithms/Python.")

(defn get-check-digit
  "Computes the EAN-13 check digit for `barcode` (last digit excluded
  from the computation, then compared against it)."
  [barcode]
  (loop [code (quot barcode 10) checker false s 0]
    (if (zero? code)
      (mod (- 10 (mod s 10)) 10)
      (recur (quot code 10) (not checker)
             (+ s (* (if checker 1 3) (mod code 10)))))))

(defn valid?
  "True when `barcode` is a 13-digit code with a correct check digit."
  [barcode]
  (and (= 13 (count (str barcode)))
       (= (get-check-digit barcode) (mod barcode 10))))

(defn get-barcode
  "Parses `barcode` to a long. Throws IllegalArgumentException for
  alphabetic or negative input."
  [barcode]
  (let [s (str barcode)]
    (when (re-matches #"[A-Za-z]+" s)
      (throw (IllegalArgumentException.
              (str "Barcode '" barcode "' has alphabetic characters."))))
    (let [n (Long/parseLong s)]
      (when (neg? n)
        (throw (IllegalArgumentException.
                "The entered barcode has a negative value. Try again.")))
      n)))
