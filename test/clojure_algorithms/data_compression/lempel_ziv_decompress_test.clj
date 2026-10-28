(ns clojure-algorithms.data-compression.lempel-ziv-decompress-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.lempel-ziv-decompress
             :refer [decompress-data remove-prefix]]))

(deftest test-decompress-data
  (is (= "010101010101" (decompress-data "00110010001110")))
  (is (= "010011001001110" (decompress-data "00100011100110001")))
  (is (= "0110" (decompress-data "00101")))
  (is (= "0" (decompress-data "0")))
  (is (= "1" (decompress-data "1"))))

(deftest test-remove-prefix
  (is (= "10101" (remove-prefix "000101110101")))
  (is (= "11" (remove-prefix "01011"))))
