(ns clojure-algorithms.conversions.ipv4-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.ipv4-conversion
             :refer [ipv4-to-decimal alt-ipv4-to-decimal decimal-to-ipv4]]))

(deftest test-ipv4-to-decimal
  (is (= 3232235521 (ipv4-to-decimal "192.168.0.1")))
  (is (= 167772415 (ipv4-to-decimal "10.0.0.255")))
  (is (= 0 (ipv4-to-decimal "0.0.0.0")))
  (is (thrown? clojure.lang.ExceptionInfo (ipv4-to-decimal "10.0.255")))
  (is (thrown? clojure.lang.ExceptionInfo (ipv4-to-decimal "10.0.0.256"))))

(deftest test-alt-ipv4-to-decimal
  (is (= 3232235521 (alt-ipv4-to-decimal "192.168.0.1")))
  (is (= 167772415 (alt-ipv4-to-decimal "10.0.0.255"))))

(deftest test-decimal-to-ipv4
  (is (= "192.168.0.1" (decimal-to-ipv4 3232235521)))
  (is (= "10.0.0.255" (decimal-to-ipv4 167772415)))
  (is (= "0.0.0.0" (decimal-to-ipv4 0)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-ipv4 -1))))
