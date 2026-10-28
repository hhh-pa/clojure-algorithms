(ns clojure-algorithms.conversions.binary-to-base64-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-base64 :refer [bin-to-base64]]))

(deftest test-bin-to-base64
  (is (= "AB" (bin-to-base64 "000001")))
  (is (= "A" (bin-to-base64 "0")))
  (is (= "B" (bin-to-base64 "1")))
  (is (= "-B" (bin-to-base64 "-1")))
  (is (= "-AB" (bin-to-base64 " -000001 ")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-base64 "0-0")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-base64 ""))))
