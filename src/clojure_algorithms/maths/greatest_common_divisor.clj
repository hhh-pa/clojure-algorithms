(ns clojure-algorithms.maths.greatest-common-divisor
  "Greatest Common Divisor.\n\nWikipedia reference: https://en.wikipedia.org/wiki/Greatest_common_divisor\n\ngcd(a, b) = gcd(a, -b) = gcd(-a, b) = gcd(-a, -b) by definition of divisibility"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare greatest-common-divisor gcd-by-iterative main)

(defn greatest-common-divisor "Calculate Greatest Common Divisor (GCD).\n>>> greatest_common_divisor(24, 40)\n8\n>>> greatest_common_divisor(1, 1)\n1\n>>> greatest_common_divisor(1, 800)\n1\n>>> greatest_common_divisor(11, 37)\n1\n>>> greatest_common_divisor(3, 5)\n1\n>>> greatest_common_divisor(16, 4)\n4\n>>> greatest_common_divisor(-3, 9)\n3\n>>> greatest_common_divisor(9, -3)\n3\n>>> greatest_common_divisor(3, -9)\n3\n>>> greatest_common_divisor(-3, -9)\n3\n>>> greatest_common_divisor(0, 0)\n0"
  [a b] (if (py-truthy (py-eq a 0)) (py-abs b) (greatest-common-divisor (py-mod b a) a)))

(defn gcd-by-iterative "Below method is more memory efficient because it does not create additional\nstack frames for recursive functions calls (as done in the above method).\n>>> gcd_by_iterative(24, 40)\n8\n>>> greatest_common_divisor(24, 40) == gcd_by_iterative(24, 40)\nTrue\n>>> gcd_by_iterative(-3, -9)\n3\n>>> gcd_by_iterative(3, -9)\n3\n>>> gcd_by_iterative(1, -800)\n1\n>>> gcd_by_iterative(11, 37)\n1\n>>> gcd_by_iterative(0, 0)\n0"
  [x y] (let [x (atom x) y (atom y)] (try (while (py-truthy @y) (let [__d1 (vec [@y (py-mod @x @y)])] (reset! x (nth __d1 0)) (reset! y (nth __d1 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-abs @x)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main "Call Greatest Common Divisor function."
  [] (let [num-1 (atom nil) num-2 (atom nil) nums (atom nil)] (try (reset! nums (py-split (do (print "Enter two integers separated by comma (,): ") (flush) (read-line)) "," -1)) (reset! num-1 (py-int (py-get @nums 0) nil)) (reset! num-2 (py-int (py-get @nums 1) nil)) (println (clojure.string/join " " [(py-str (str "greatest_common_divisor(" (py-str @num-1) ", " (py-str @num-2) ") = " (py-str (greatest-common-divisor @num-1 @num-2))))])) (println (clojure.string/join " " [(py-str (str "By iterative gcd(" (py-str @num-1) ", " (py-str @num-2) ") = " (py-str (gcd-by-iterative @num-1 @num-2))))])) (catch clojure.lang.ExceptionInfo __eh (if (:__control__ (ex-data __eh)) (throw __eh) (let [__t (:type (ex-data __eh))] (cond (contains? #{:IndexError :UnboundLocalError :ValueError} __t) (do (println (clojure.string/join " " [(py-str "Wrong input")]))) :else (throw __eh)))))) nil))
