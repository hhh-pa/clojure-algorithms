(ns clojure-algorithms.maths.number-of-digits
  "Port of maths/number_of_digits.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare num-digits num-digits-fast num-digits-faster benchmark)

(defn num-digits "Find the number of digits in a number.\n\n>>> num_digits(12345)\n5\n>>> num_digits(123)\n3\n>>> num_digits(0)\n1\n>>> num_digits(-1)\n1\n>>> num_digits(-123456)\n6\n>>> num_digits('123')  # Raises a TypeError for non-integer input\nTraceback (most recent call last):\n    ...\nTypeError: Input must be an integer"
  [n] (let [n (atom n)] (let [digits (atom nil)] (try (when (py-truthy (not (py-truthy (integer? @n)))) (throw (ex-info (py-str "Input must be an integer") {:type :TypeError}))) (reset! digits 0) (reset! n (py-abs @n)) (try (while (py-truthy true) (reset! n (py-floordiv @n 10)) (swap! digits py-add 1) (when (py-truthy (py-eq @n 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @digits})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn num-digits-fast "Find the number of digits in a number.\nabs() is used as logarithm for negative numbers is not defined.\n\n>>> num_digits_fast(12345)\n5\n>>> num_digits_fast(123)\n3\n>>> num_digits_fast(1000)\n4\n>>> num_digits_fast(10**15)\n16\n>>> num_digits_fast(0)\n1\n>>> num_digits_fast(-1)\n1\n>>> num_digits_fast(-123456)\n6\n>>> num_digits_fast('123')  # Raises a TypeError for non-integer input\nTraceback (most recent call last):\n    ...\nTypeError: Input must be an integer"
  [n] (try (when (py-truthy (not (py-truthy (integer? n)))) (throw (ex-info (py-str "Input must be an integer") {:type :TypeError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-eq n 0)) 1 (py-floor (py-add (Math/log10 (py-abs n)) 1)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn num-digits-faster "Find the number of digits in a number.\nabs() is used for negative numbers.\n\n>>> num_digits_faster(12345)\n5\n>>> num_digits_faster(123)\n3\n>>> num_digits_faster(0)\n1\n>>> num_digits_faster(-1)\n1\n>>> num_digits_faster(-123456)\n6\n>>> num_digits_faster('123')  # Raises a TypeError for non-integer input\nTraceback (most recent call last):\n    ...\nTypeError: Input must be an integer"
  [n] (try (when (py-truthy (not (py-truthy (integer? n)))) (throw (ex-info (py-str "Input must be an integer") {:type :TypeError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (count (py-str (py-abs n)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn benchmark "Benchmark multiple functions with three different integer sizes."
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
