(ns clojure-algorithms.maths.twin-prime
  "== Twin Prime ==\nA number n+2 is said to be a Twin prime of number n if\nboth n and n+2 are prime.\n\nExamples of Twin pairs: (3, 5), (5, 7), (11, 13), (17, 19), (29, 31), (41, 43), ...\nhttps://en.wikipedia.org/wiki/Twin_prime"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure-algorithms.maths.prime-check :as m1]))

(declare twin-prime)

(defn twin-prime "# doctest: +NORMALIZE_WHITESPACE\nThis functions takes an integer number as input.\nreturns n+2 if n and n+2 are prime numbers and -1 otherwise.\n>>> twin_prime(3)\n5\n>>> twin_prime(4)\n-1\n>>> twin_prime(5)\n7\n>>> twin_prime(17)\n19\n>>> twin_prime(0)\n-1\n>>> twin_prime(6.0)\nTraceback (most recent call last):\n    ...\nTypeError: Input value of [number=6.0] must be an integer"
  [number] (let [msg (atom nil)] (try (when (py-truthy (not (py-truthy (integer? number)))) (reset! msg (str "Input value of [number=" (py-str number) "] must be an integer")) (throw (ex-info (py-str @msg) {:type :TypeError}))) (if (py-truthy (let [__b1 (m1/is-prime number)] (if (py-truthy __b1) (m1/is-prime (py-add number 2)) __b1))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add number 2)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-neg 1)})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
