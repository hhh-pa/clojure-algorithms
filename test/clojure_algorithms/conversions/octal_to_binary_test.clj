(ns clojure-algorithms.conversions.octal-to-binary-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.octal-to-binary :refer [octal-to-binary]]))

(deftest test-octal-to-binary
  (is (= "001111" (octal-to-binary "17")))
  (is (= "111" (octal-to-binary "7")))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-binary "Av")))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-binary "@#")))
  (is (thrown? clojure.lang.ExceptionInfo (octal-to-binary ""))))
