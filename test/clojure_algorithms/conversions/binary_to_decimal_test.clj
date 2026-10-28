(ns clojure-algorithms.conversions.binary-to-decimal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-decimal :refer [bin-to-decimal]]))

(deftest test-bin-to-decimal
  (is (= 5 (bin-to-decimal "101")))
  (is (= 10 (bin-to-decimal " 1010   ")))
  (is (= -29 (bin-to-decimal "-11101")))
  (is (= 0 (bin-to-decimal "0")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-decimal "a")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-decimal "")))
  (is (thrown? clojure.lang.ExceptionInfo (bin-to-decimal "39"))))
