(ns clojure-algorithms.conversions.binary-to-octal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-octal :refer [bin-to-octal]]))

(deftest test-bin-to-octal
  (is (= "17" (bin-to-octal "1111")))
  (is (= "52523" (bin-to-octal "101010101010011")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-octal "")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-octal "a-1"))))
