(ns clojure-algorithms.maths.factorial
  "Factorial of a positive integer -- https://en.wikipedia.org/wiki/Factorial")

(defn factorial
  "Calculate the factorial of the specified number."
  [number]
  (when (not (number? number))
    (throw (ex-info "factorial() only accepts integral values" {:type :ValueError})))
  (when (not (== number (long number)))
    (throw (ex-info "factorial() only accepts integral values" {:type :ValueError})))
  (when (neg? number)
    (throw (ex-info "factorial() not defined for negative values" {:type :ValueError})))
  (when (not (integer? number))
    (throw (ex-info "'float' object cannot be interpreted as an integer"
                    {:type :TypeError})))
  (reduce * 1 (range 1 (inc (long number)))))

(defn factorial-recursive
  "Calculate the factorial of a number using recursion."
  [number]
  (when (not (integer? number))
    (throw (ex-info "factorial_recursive() only accepts integral values"
                    {:type :ValueError})))
  (when (neg? number)
    (throw (ex-info "factorial_recursive() not defined for negative values"
                    {:type :ValueError})))
  (if (<= number 1)
    1
    (* number (factorial-recursive (dec number)))))
