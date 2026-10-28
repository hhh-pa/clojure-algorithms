(ns clojure-algorithms.conversions.octal-to-decimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.octal-to-decimal :refer [oct-to-decimal]]))

(deftest test-oct-to-decimal
  (is (= 1 (oct-to-decimal "1")))
  (is (= -1 (oct-to-decimal "-1")))
  (is (= 10 (oct-to-decimal "12")))
  (is (= 10 (oct-to-decimal " 12   ")))
  (is (= -37 (oct-to-decimal "-45")))
  (is (= 0 (oct-to-decimal "0")))
  (is (= -2093 (oct-to-decimal "-4055")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "-")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "e")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "8")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "2-0Fm")))
  (is (thrown? clojure.lang.ExceptionInfo (oct-to-decimal "19"))))
