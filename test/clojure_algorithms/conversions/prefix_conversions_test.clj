(ns clojure-algorithms.conversions.prefix-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.prefix-conversions
             :refer [convert-si-prefix convert-binary-prefix]]))

(deftest test-convert-si-prefix
  (is (== 1000 (convert-si-prefix 1 :giga :mega)))
  (is (== 0.001 (convert-si-prefix 1 :mega :giga)))
  (is (== 1 (convert-si-prefix 1 :kilo :kilo)))
  (is (== 1000 (convert-si-prefix 1 "giga" "mega")))
  (is (== 1000 (convert-si-prefix 1 "gIGa" "mEGa"))))

(deftest test-convert-binary-prefix
  (is (== 1024 (convert-binary-prefix 1 :giga :mega)))
  (is (== 0.0009765625 (convert-binary-prefix 1 :mega :giga)))
  (is (== 1 (convert-binary-prefix 1 :kilo :kilo)))
  (is (== 1024 (convert-binary-prefix 1 "giga" "mega")))
  (is (== 1024 (convert-binary-prefix 1 "gIGa" "mEGa"))))
