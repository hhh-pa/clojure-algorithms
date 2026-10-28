(ns clojure-algorithms.maths.numerical-analysis.proper-fractions
  "List the proper fractions between 0 and 1 for a given denominator.
https://en.wikipedia.org/wiki/Fraction#Proper_and_improper_fractions")

(defn- gcd
  [a b]
  (loop [a (Math/abs (long a))
         b (Math/abs (long b))]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn proper-fractions
  "Return the proper fractions between 0 and 1 with the given denominator.

  Throws ex-info with :type :ValueError when the denominator is negative or
  not an integer."
  [denominator]
  (when (< denominator 0)
    (throw (ex-info "The Denominator Cannot be less than 0"
                    {:type :ValueError})))
  (when-not (integer? denominator)
    (throw (ex-info "The Denominator must be an integer"
                    {:type :ValueError})))
  (vec (for [numerator (range 1 denominator)
             :when (= 1 (gcd numerator denominator))]
         (str numerator "/" denominator))))
