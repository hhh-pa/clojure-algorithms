(ns clojure-algorithms.conversions.hexadecimal-to-decimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.hexadecimal-to-decimal
             :refer [hex-to-decimal]]))

(deftest test-hex-to-decimal
  (is (= 10 (hex-to-decimal "a")))
  (is (= 303 (hex-to-decimal "12f")))
  (is (= 303 (hex-to-decimal "   12f   ")))
  (is (= 65535 (hex-to-decimal "FfFf")))
  (is (= -255 (hex-to-decimal "-Ff")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-decimal "F-f")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-decimal "")))
  (is (thrown? clojure.lang.ExceptionInfo (hex-to-decimal "12m"))))
