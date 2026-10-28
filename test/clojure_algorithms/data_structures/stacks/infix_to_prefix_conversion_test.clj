(ns clojure-algorithms.data-structures.stacks.infix-to-prefix-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.infix-to-prefix-conversion
             :refer [infix-2-postfix infix-2-prefix]]))

(deftest test-infix-2-postfix
  (is (= "abc^+" (infix-2-postfix "a+b^c")))
  (is (= "1a-2*b+*" (infix-2-postfix "1*((-a)*2+b)")))
  (is (= "" (infix-2-postfix "")))
  (is (thrown? clojure.lang.ExceptionInfo (infix-2-postfix "(()")))
  (is (thrown? clojure.lang.ExceptionInfo (infix-2-postfix "())"))))

(deftest test-infix-2-prefix
  (is (= "+a^bc" (infix-2-prefix "a+b^c")))
  (is (= "*1+*-a2b" (infix-2-prefix "1*((-a)*2+b)")))
  (is (= "" (infix-2-prefix "")))
  (is (thrown? clojure.lang.ExceptionInfo (infix-2-prefix "(()")))
  (is (thrown? clojure.lang.ExceptionInfo (infix-2-prefix "())"))))
