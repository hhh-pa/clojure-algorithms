(ns clojure-algorithms.maths.is-int-palindrome
  "Port of maths/is_int_palindrome.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare is-int-palindrome)

(defn is-int-palindrome "Returns whether `num` is a palindrome or not\n(see for reference https://en.wikipedia.org/wiki/Palindromic_number).\n\n>>> is_int_palindrome(-121)\nFalse\n>>> is_int_palindrome(0)\nTrue\n>>> is_int_palindrome(10)\nFalse\n>>> is_int_palindrome(11)\nTrue\n>>> is_int_palindrome(101)\nTrue\n>>> is_int_palindrome(120)\nFalse"
  [num] (let [num (atom num)] (let [num-copy (atom nil) rev-num (atom nil)] (try (when (py-truthy (py-lt @num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (reset! num-copy @num) (reset! rev-num 0) (while (py-truthy (py-gt @num 0)) (reset! rev-num (py-add (py-mul @rev-num 10) (py-mod @num 10))) (swap! num py-floordiv 10)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @num-copy @rev-num)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
