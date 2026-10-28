(ns clojure-algorithms.conversions.binary-to-gray-code-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-gray-code
             :refer [binary-to-gray gray-to-binary decimal-to-gray gray-to-decimal]]))

(deftest test-binary-to-gray
  (is (= 0 (binary-to-gray 0)))
  (is (= 1 (binary-to-gray 1)))
  (is (= 3 (binary-to-gray 2)))
  (is (= 2 (binary-to-gray 3)))
  (is (= 6 (binary-to-gray 4)))
  (is (= 4 (binary-to-gray 7)))
  (is (= 15 (binary-to-gray 10)))
  (is (= 8 (binary-to-gray 15)))
  (is (= 128 (binary-to-gray 255)))
  (is (thrown? clojure.lang.ExceptionInfo (binary-to-gray -1)))
  (is (thrown? clojure.lang.ExceptionInfo (binary-to-gray 3.5))))

(deftest test-gray-to-binary
  (is (= 0 (gray-to-binary 0)))
  (is (= 1 (gray-to-binary 1)))
  (is (= 2 (gray-to-binary 3)))
  (is (= 3 (gray-to-binary 2)))
  (is (= 4 (gray-to-binary 6)))
  (is (= 7 (gray-to-binary 4)))
  (is (= 10 (gray-to-binary 15)))
  (is (= 15 (gray-to-binary 8)))
  (is (= 255 (gray-to-binary 128)))
  (is (thrown? clojure.lang.ExceptionInfo (gray-to-binary -1)))
  (is (thrown? clojure.lang.ExceptionInfo (gray-to-binary 5.5))))

(deftest test-decimal-to-gray-and-back
  (is (= "0" (decimal-to-gray 0)))
  (is (= "1" (decimal-to-gray 1)))
  (is (= "11" (decimal-to-gray 2)))
  (is (= "10" (decimal-to-gray 3)))
  (is (= "110" (decimal-to-gray 4)))
  (is (= "1111" (decimal-to-gray 10)))
  (is (= "1000" (decimal-to-gray 15)))
  (is (thrown? clojure.lang.ExceptionInfo (decimal-to-gray -1)))
  (is (= 0 (gray-to-decimal "0")))
  (is (= 1 (gray-to-decimal "1")))
  (is (= 2 (gray-to-decimal "11")))
  (is (= 3 (gray-to-decimal "10")))
  (is (= 4 (gray-to-decimal "110")))
  (is (= 10 (gray-to-decimal "1111")))
  (is (= 15 (gray-to-decimal "1000")))
  (is (thrown? clojure.lang.ExceptionInfo (gray-to-decimal "invalid")))
  (is (thrown? clojure.lang.ExceptionInfo (gray-to-decimal ""))))
