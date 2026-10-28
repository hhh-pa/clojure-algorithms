(ns clojure-algorithms.dynamic-programming.fizz-buzz
  "FizzBuzz from `number` up to `iterations` (inclusive), space-separated.
  Ported from TheAlgorithms/Python dynamic_programming/fizz_buzz.py.")

(defn fizz-buzz
  "FizzBuzz string for integers `number`..`iterations`."
  [number iterations]
  (when-not (integer? iterations)
    (throw (IllegalArgumentException. "iterations must be defined as integers")))
  (when-not (and (integer? number) (>= number 1))
    (throw (IllegalArgumentException.
            "starting number must be\n                         and integer and be more than 0")))
  (when-not (>= iterations 1)
    (throw (IllegalArgumentException.
            "Iterations must be done more than 0 times to play FizzBuzz")))
  (apply str (for [n (range number (inc iterations))]
               (str (cond
                      (zero? (mod n 15)) "FizzBuzz"
                      (zero? (mod n 3)) "Fizz"
                      (zero? (mod n 5)) "Buzz"
                      :else n)
                    " "))))
