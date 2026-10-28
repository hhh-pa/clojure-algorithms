(ns clojure-algorithms.conversions.decimal-to-binary-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.decimal-to-binary
             :refer [decimal-to-binary-iterative decimal-to-binary-recursive-helper
                     decimal-to-binary-recursive]]))

(deftest test-iterative
  (is (= "0b0" (decimal-to-binary-iterative 0)))
  (is (= "0b10" (decimal-to-binary-iterative 2)))
  (is (= "0b111" (decimal-to-binary-iterative 7)))
  (is (= "0b100011" (decimal-to-binary-iterative 35)))
  (is (= "-0b10" (decimal-to-binary-iterative -2)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-binary-iterative 16.16)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-binary-iterative "0xfffff"))))

(deftest test-recursive-helper
  (is (= "1111101000" (decimal-to-binary-recursive-helper 1000)))
  (is (= "1001000" (decimal-to-binary-recursive-helper "72")))
  (is (thrown? clojure.lang.ExceptionInfo
               (decimal-to-binary-recursive-helper "number"))))

(deftest test-recursive
  (is (= "0b0" (decimal-to-binary-recursive 0)))
  (is (= "0b101000" (decimal-to-binary-recursive 40)))
  (is (= "-0b101000" (decimal-to-binary-recursive -40)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-binary-recursive 40.8)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-binary-recursive "forty"))))
