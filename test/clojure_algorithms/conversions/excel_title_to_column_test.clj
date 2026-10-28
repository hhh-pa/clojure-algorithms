(ns clojure-algorithms.conversions.excel-title-to-column-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.excel-title-to-column
             :refer [excel-title-to-column]]))

(deftest test-excel-title-to-column
  (is (= 1 (excel-title-to-column "A")))
  (is (= 2 (excel-title-to-column "B")))
  (is (= 28 (excel-title-to-column "AB")))
  (is (= 26 (excel-title-to-column "Z")))
  (is (= 1 (excel-title-to-column "a")))
  (is (= 28 (excel-title-to-column "ab")))
  (is (= 703 (excel-title-to-column "AAA")))
  (is (thrown? clojure.lang.ExceptionInfo (excel-title-to-column "")))
  (is (thrown? clojure.lang.ExceptionInfo (excel-title-to-column "A1"))))
