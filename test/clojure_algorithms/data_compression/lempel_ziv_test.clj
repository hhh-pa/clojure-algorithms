(ns clojure-algorithms.data-compression.lempel-ziv-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.lempel-ziv
             :refer [compress-data add-key-to-lexicon]]))

(deftest test-compress-data
  (is (= "00100011100110001" (compress-data "01001100100111")))
  (is (= "00000000" (compress-data "00000000")))
  (is (= "00110010001110" (compress-data "010101010101")))
  (is (= "00101" (compress-data "0110")))
  (is (= "0" (compress-data "0")))
  (is (= "1" (compress-data "1")))
  (is (every? #{\0 \1} (compress-data "1111000011110000"))))

(deftest test-add-key-to-lexicon
  (let [lex (add-key-to-lexicon {"0" "0" "1" "1"} "0" 2 "0")]
    (is (= "00" (get lex "00")))
    (is (= "10" (get lex "01")))))
