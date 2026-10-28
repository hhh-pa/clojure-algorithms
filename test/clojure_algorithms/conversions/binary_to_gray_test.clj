(ns clojure-algorithms.conversions.binary-to-gray-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.binary-to-gray :refer [binary-to-gray]]))

(deftest test-binary-to-gray
  (is (= "11111111" (binary-to-gray "10101010")))
  (is (= "1011" (binary-to-gray "1101")))
  (is (= "0" (binary-to-gray "0")))
  (is (= "1" (binary-to-gray "1"))))
