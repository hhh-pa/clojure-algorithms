(ns clojure-algorithms.conversions.endianness-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.endianness
             :refer [swap-endianness-16 swap-endianness-32 swap-endianness-64
                     bytes-to-int-little bytes-to-int-big
                     int-to-bytes-little int-to-bytes-big]]))

(deftest test-swap-16
  (is (= 13330 (swap-endianness-16 0x1234)))
  (is (= "0x3412" (format "0x%x" (swap-endianness-16 0x1234))))
  (is (= 52651 (swap-endianness-16 0xABCD)))
  (is (= 0 (swap-endianness-16 0)))
  (is (= 65535 (swap-endianness-16 0xFFFF)))
  (is (thrown? clojure.lang.ExceptionInfo (swap-endianness-16 -1)))
  (is (thrown? clojure.lang.ExceptionInfo (swap-endianness-16 0x10000))))

(deftest test-swap-32
  (is (= 2018915346 (swap-endianness-32 0x12345678)))
  (is (= "0x78563412" (format "0x%x" (swap-endianness-32 0x12345678))))
  (is (= 4022250974 (swap-endianness-32 0xDEADBEEF)))
  (is (= 0 (swap-endianness-32 0)))
  (is (= 4294967295 (swap-endianness-32 0xFFFFFFFF)))
  (is (= 67305985 (swap-endianness-32 0x01020304))))

(deftest test-swap-64
  (is (= 17279655951921914625 (swap-endianness-64 0x0123456789ABCDEF)))
  (is (= 1167088121787636990 (swap-endianness-64 0xFEDCBA9876543210)))
  (is (= 0 (swap-endianness-64 0)))
  (is (= 18446744073709551615 (swap-endianness-64 0xFFFFFFFFFFFFFFFF))))

(deftest test-bytes-int-roundtrip
  (is (= 305419896 (bytes-to-int-little [0x78 0x56 0x34 0x12])))
  (is (= 513 (bytes-to-int-little [0x01 0x02])))
  (is (= 255 (bytes-to-int-little [0xFF])))
  (is (= 16777216 (bytes-to-int-little [0x00 0x00 0x00 0x01])))
  (is (= 305419896 (bytes-to-int-big [0x12 0x34 0x56 0x78])))
  (is (= 258 (bytes-to-int-big [0x01 0x02])))
  (is (= 255 (bytes-to-int-big [0xFF])))
  (is (= 1 (bytes-to-int-big [0x00 0x00 0x00 0x01])))
  (is (= [0x78 0x56 0x34 0x12] (int-to-bytes-little 0x12345678 4)))
  (is (= [0x01 0x02] (int-to-bytes-little 513 2)))
  (is (= [0xFF] (int-to-bytes-little 255 1)))
  (is (= [0x00 0x00 0x00 0x01] (int-to-bytes-little 16777216 4)))
  (is (= [0x12 0x34 0x56 0x78] (int-to-bytes-big 0x12345678 4)))
  (is (= [0x01 0x02] (int-to-bytes-big 258 2)))
  (is (= [0xFF] (int-to-bytes-big 255 1)))
  (is (= [0x00 0x00 0x00 0x01] (int-to-bytes-big 1 4)))
  (is (thrown? clojure.lang.ExceptionInfo (bytes-to-int-little [])))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-bytes-big -1 2)))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-bytes-big 256 1))))
