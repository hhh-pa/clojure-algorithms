(ns clojure-algorithms.data-structures.stacks.postfix-to-infix-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.postfix-to-infix-conversion
             :refer [postfix-to-infix]]))

(deftest test-postfix-to-infix
  (is (thrown? clojure.lang.ExceptionInfo (postfix-to-infix "")))
  (is (thrown? clojure.lang.ExceptionInfo (postfix-to-infix nil)))
  (is (= "((1*(2+3))+4)" (postfix-to-infix "123+*4+")))
  (is (= "((a+(b*c))+(((d*e)+f)*g))" (postfix-to-infix "abc*+de*f+g*+")))
  (is (= "(((x^y)/(5*z))+2)" (postfix-to-infix "xy^5z*/2+")))
  (is (= "(2^(3^2))" (postfix-to-infix "232^^")))
  (is (= "(3+2)" (postfix-to-infix "32+"))))
