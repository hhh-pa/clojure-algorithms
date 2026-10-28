(ns clojure-algorithms.conversions.decimal-to-octal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.decimal-to-octal :refer [decimal-to-octal]]))

(deftest test-decimal-to-octal
  (is (= "0o0" (decimal-to-octal 0)))
  (is (= "0o2" (decimal-to-octal 2)))
  (is (= "0o10" (decimal-to-octal 8)))
  (is (= "0o100" (decimal-to-octal 64)))
  (is (= "0o101" (decimal-to-octal 65)))
  (is (= "0o330" (decimal-to-octal 216)))
  (is (= "0o377" (decimal-to-octal 255)))
  (is (= "0o400" (decimal-to-octal 256)))
  (is (= "0o1000" (decimal-to-octal 512))))
