(ns clojure-algorithms.data-structures.stacks.infix-to-postfix-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.infix-to-postfix-conversion
             :refer [precedence associativity infix-to-postfix]]))

(deftest test-infix-to-postfix
  (is (thrown? clojure.lang.ExceptionInfo (infix-to-postfix "(1*(2+3)+4))")))
  (is (= "" (infix-to-postfix "")))
  (is (= "3 2 +" (infix-to-postfix "3+2")))
  (is (= "3 4 + 5 * 6 -" (infix-to-postfix "(3+4)*5-6")))
  (is (= "1 2 + 3 * 4 / 5 -" (infix-to-postfix "(1+2)*3/4-5")))
  (is (= "a b c * + d e * f + g * +" (infix-to-postfix "a+b*c+(d*e+f)*g")))
  (is (= "x y ^ 5 z * / 2 +" (infix-to-postfix "x^y/(5*z)+2")))
  (is (= "2 3 2 ^ ^" (infix-to-postfix "2^3^2")))
  (is (= 3 (precedence \^)))
  (is (= -1 (precedence \a)))
  (is (= "RL" (associativity \^)))
  (is (= "LR" (associativity \+))))
