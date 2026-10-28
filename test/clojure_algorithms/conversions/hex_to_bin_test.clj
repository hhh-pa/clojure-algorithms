(ns clojure-algorithms.conversions.hex-to-bin-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.hex-to-bin :refer [hex-to-bin]]))

(deftest test-hex-to-bin
  (is (= 10101100 (hex-to-bin "AC")))
  (is (= 100110100100 (hex-to-bin "9A4")))
  (is (= 100101111 (hex-to-bin "   12f   ")))
  (is (= 1111111111111111 (hex-to-bin "FfFf")))
  (is (= -1111111111111111 (hex-to-bin "-fFfF")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-bin "F-f")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-bin ""))))
