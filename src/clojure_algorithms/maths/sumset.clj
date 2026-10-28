(ns clojure-algorithms.maths.sumset
  "Calculates the SumSet of two sets of numbers (A and B)\n\nSource:\n    https://en.wikipedia.org/wiki/Sumset"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare sumset)

(defn sumset ":param set_a: a set of numbers\n:param set_b: a set of numbers\n:return: the sumset of set_a and set_b (all pairwise sums a + b)\n\n>>> sumset({1, 2, 3}, {4, 5, 6})\n{5, 6, 7, 8, 9}\n\n>>> sumset({1, 2, 3}, {4, 5, 6, 7})\n{5, 6, 7, 8, 9, 10}\n\n>>> sumset({1, 2, 3, 4}, 3)\nTraceback (most recent call last):\n...\nAssertionError: The input value of [set_b=3] is not a set\n>>> sumset(set(), {1, 2, 3})\nset()"
  [set-a set-b] (try (assert (py-truthy (set? set-a)) (py-str (str "The input value of [set_a=" (py-str set-a) "] is not a set"))) (assert (py-truthy (set? set-b)) (py-str (str "The input value of [set_b=" (py-str set-b) "] is not a set"))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (set (for [a (py-iter set-a) b (py-iter set-b)] (py-add a b)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
