(ns clojure-algorithms.conversions.base64-to-binary-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.base64-to-binary :refer [base64-to-bin]]))

(deftest test-base64-to-bin
  (is (= "000000000001000010" (base64-to-bin "ABC")))
  (is (= "-011010011011011100" (base64-to-bin "    -abc    ")))
  (is (thrown? clojure.lang.ExceptionInfo (base64-to-bin "    a-bc    ")))
  (is (thrown? clojure.lang.ExceptionInfo (base64-to-bin ""))))
