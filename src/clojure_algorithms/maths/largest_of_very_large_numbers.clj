(ns clojure-algorithms.maths.largest-of-very-large-numbers
  "Compare very large powers by reducing x^y to the manageable
number y*log10(x).")

(defn res
  "Reduces a large power x^y to a more manageable number: y*log10(x).
0 raised to any number is 0; any number raised to 0 is 1.
A negative base raises :ValueError (math domain error), matching
Python's math.log10 on negative input."
  [x y]
  (cond
    (and (not (zero? x)) (not (zero? y)))
    (if (neg? x)
      (throw (ex-info "math domain error" {:type :ValueError}))
      (* y (Math/log10 x)))
    (zero? x) 0
    (zero? y) 1))
