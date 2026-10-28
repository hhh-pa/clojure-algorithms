(ns clojure-algorithms.conversions.decimal-to-hexadecimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.decimal-to-hexadecimal
             :refer [decimal-to-hexadecimal]]))

(deftest test-decimal-to-hexadecimal
  (is (= "0x5" (decimal-to-hexadecimal 5)))
  (is (= "0xf" (decimal-to-hexadecimal 15)))
  (is (= "0x25" (decimal-to-hexadecimal 37)))
  (is (= "0xff" (decimal-to-hexadecimal 255)))
  (is (= "0x1000" (decimal-to-hexadecimal 4096)))
  (is (= "0xf3eba" (decimal-to-hexadecimal 999098)))
  (is (= "-0x100" (decimal-to-hexadecimal -256)))
  (is (= "0x11" (decimal-to-hexadecimal 17.0)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-hexadecimal 16.16)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-hexadecimal "0xfffff"))))
