(ns clojure-algorithms.conversions.binary-to-hexadecimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-hexadecimal
             :refer [bin-to-hexadecimal]]))

(deftest test-bin-to-hexadecimal
  (is (= "0x15f" (bin-to-hexadecimal "101011111")))
  (is (= "0x0a" (bin-to-hexadecimal " 1010   ")))
  (is (= "-0x1d" (bin-to-hexadecimal "-11101")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-hexadecimal "a")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-hexadecimal ""))))
