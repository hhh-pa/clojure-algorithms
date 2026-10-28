(ns clojure-algorithms.conversions.prefix-conversions-string-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.prefix-conversions-string
             :refer [add-si-prefix add-binary-prefix]]))

(deftest test-add-si-prefix
  (is (= "10.0 kilo" (add-si-prefix 10000)))
  (is (= "5" (add-si-prefix 5)))
  (is (= "-10000" (add-si-prefix -10000)))
  (is (= "1.5 mega" (add-si-prefix 1500000))))

(deftest test-add-binary-prefix
  (is (= "64.0 kilo" (add-binary-prefix 65536)))
  (is (= "100" (add-binary-prefix 100)))
  (is (= "2.0 mega" (add-binary-prefix (* 2 1024 1024)))))
