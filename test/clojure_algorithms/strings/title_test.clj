(ns clojure-algorithms.strings.title-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.title :refer [to-title-case
                                                      sentence-to-title-case]]))

(deftest test-to-title-case
  (is (= "Aakash" (to-title-case "Aakash")))
  (is (= "Aakash" (to-title-case "aakash")))
  (is (= "Aakash" (to-title-case "AAKASH")))
  (is (= "Aakash" (to-title-case "aAkAsH"))))

(deftest test-sentence-to-title-case
  (is (= "Aakash Giri" (sentence-to-title-case "Aakash Giri")))
  (is (= "Aakash Giri" (sentence-to-title-case "aakash giri")))
  (is (= "Aakash Giri" (sentence-to-title-case "AAKASH GIRI")))
  (is (= "Aakash Giri" (sentence-to-title-case "aAkAsH gIrI"))))
