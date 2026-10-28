(ns clojure-algorithms.conversions.roman-numerals-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.roman-numerals
             :refer [roman-to-int int-to-roman]]))

(deftest test-roman-to-int
  (is (= 3 (roman-to-int "III")))
  (is (= 154 (roman-to-int "CLIV")))
  (is (= 1009 (roman-to-int "MIX")))
  (is (= 2500 (roman-to-int "MMD")))
  (is (= 3999 (roman-to-int "MMMCMXCIX"))))

(deftest test-int-to-roman
  (is (= "III" (int-to-roman 3)))
  (is (= "CLIV" (int-to-roman 154)))
  (is (= "MIX" (int-to-roman 1009)))
  (is (= "MMD" (int-to-roman 2500)))
  (is (= "MMMCMXCIX" (int-to-roman 3999)))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-roman 0)))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-roman -1)))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-roman 4000)))
  (is (thrown? clojure.lang.ExceptionInfo (int-to-roman true))))
