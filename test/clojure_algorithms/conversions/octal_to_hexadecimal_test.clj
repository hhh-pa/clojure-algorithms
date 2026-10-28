(ns clojure-algorithms.conversions.octal-to-hexadecimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.octal-to-hexadecimal :refer [octal-to-hex]]))

(deftest test-octal-to-hex
  (is (= "0x40" (octal-to-hex "100")))
  (is (= "0x9D" (octal-to-hex "235")))
  (is (= "0x18" (octal-to-hex "030")))
  (is (= "0xA7" (octal-to-hex "247")))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-hex 17)))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-hex "Av")))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-hex ""))))
