(ns clojure-algorithms.maths.sum-of-digits
  "Port of maths/sum_of_digits.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare sum-of-digits sum-of-digits-recursion sum-of-digits-compact benchmark)

(defn sum-of-digits "Find the sum of digits of a number.\n>>> sum_of_digits(12345)\n15\n>>> sum_of_digits(123)\n6\n>>> sum_of_digits(-123)\n6\n>>> sum_of_digits(0)\n0"
  [n] (let [n (atom n)] (let [res (atom nil)] (try (reset! n (py-abs @n)) (reset! res 0) (while (py-truthy (py-gt @n 0)) (swap! res py-add (py-mod @n 10)) (swap! n py-floordiv 10)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn sum-of-digits-recursion "Find the sum of digits of a number using recursion\n>>> sum_of_digits_recursion(12345)\n15\n>>> sum_of_digits_recursion(123)\n6\n>>> sum_of_digits_recursion(-123)\n6\n>>> sum_of_digits_recursion(0)\n0"
  [n] (let [n (atom n)] (try (reset! n (py-abs @n)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-lt @n 10)) @n (py-add (py-mod @n 10) (sum-of-digits-recursion (py-floordiv @n 10))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn sum-of-digits-compact "Find the sum of digits of a number\n>>> sum_of_digits_compact(12345)\n15\n>>> sum_of_digits_compact(123)\n6\n>>> sum_of_digits_compact(-123)\n6\n>>> sum_of_digits_compact(0)\n0"
  [n] (reduce +' 0 (doall (for [c (py-iter (py-str (py-abs n)))] (py-int c nil)))))

(defn benchmark "Benchmark multiple functions, with three different length int values."
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
