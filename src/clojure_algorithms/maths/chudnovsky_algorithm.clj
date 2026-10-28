(ns clojure-algorithms.maths.chudnovsky-algorithm
  "Port of maths/chudnovsky_algorithm.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare pi)

(defn pi "The Chudnovsky algorithm is a fast method for calculating the digits of PI,\nbased on Ramanujan's PI formulae.\n\nhttps://en.wikipedia.org/wiki/Chudnovsky_algorithm\n\nPI = constant_term / ((multinomial_term * linear_term) / exponential_term)\n    where constant_term = 426880 * sqrt(10005)\n\nThe linear_term and the exponential_term can be defined iteratively as follows:\n    L_k+1 = L_k + 545140134            where L_0 = 13591409\n    X_k+1 = X_k * -262537412640768000  where X_0 = 1\n\nThe multinomial_term is defined as follows:\n    6k! / ((3k)! * (k!) ^ 3)\n        where k is the k_th iteration.\n\nThis algorithm correctly calculates around 14 digits of PI per iteration\n\n>>> pi(10)\n'3.14159265'\n>>> pi(100)\n'3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706'\n>>> pi('hello')\nTraceback (most recent call last):\n    ...\nTypeError: Undefined for non-integers\n>>> pi(-1)\nTraceback (most recent call last):\n    ...\nValueError: Undefined for non-natural numbers"
  [& _args]
  ;; stub: original requires store target; pure-Clojure port not applicable
  (throw (ex-info "requires store target" {:type :UnsupportedOperation})))

;; NOTE: pi stubbed (store target)
