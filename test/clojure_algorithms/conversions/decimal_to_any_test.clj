(ns clojure-algorithms.conversions.decimal-to-any-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.decimal-to-any :refer [decimal-to-any]]))

(deftest test-decimal-to-any
  (is (= "0" (decimal-to-any 0 2)))
  (is (= "11" (decimal-to-any 5 4)))
  (is (= "202" (decimal-to-any 20 3)))
  (is (= "3A" (decimal-to-any 58 16)))
  (is (= "E5" (decimal-to-any 243 17)))
  (is (= "QY3" (decimal-to-any 34923 36)))
  (is (= "A" (decimal-to-any 10 11)))
  (is (= "10" (decimal-to-any 16 16)))
  (is (= "10" (decimal-to-any 36 36)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any -45 8)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any 34.4 6)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any 5 2.5)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any 10 "16")))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any 7 0)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-any 34 37))))
