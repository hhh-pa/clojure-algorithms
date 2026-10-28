(ns clojure-algorithms.maths.decimal-isolate
  "Isolate the Decimal part of a Number.
https://stackoverflow.com/questions/3886402/how-to-get-numbers-after-decimal-point")

(defn- round-half-even
  "Round-half-even the exact binary value of x to n decimal places,
matching Python's round()."
  [x n]
  (-> (BigDecimal. ^double (double x))
      (.setScale (int n) java.math.RoundingMode/HALF_EVEN)
      (.doubleValue)))

(defn decimal-isolate
  "Isolates the decimal part of a number. If digit-amount is positive, round
to that many decimal places, else return the entire decimal part."
  [number digit-amount]
  (let [truncated (if (integer? number) number (long number))
        frac (- number truncated)]
    (if (pos? digit-amount)
      (round-half-even frac digit-amount)
      frac)))
