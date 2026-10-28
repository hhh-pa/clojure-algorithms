(ns clojure-algorithms.strings.string-is-valid-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.string-is-valid-number
             :refer [classify-char is-valid-number]]))

(deftest test-classify-char
  (is (= :numeric (classify-char "2")))
  (is (= :sign (classify-char "-")))
  (is (= :exponent (classify-char "e")))
  (is (= :decimal (classify-char ".")))
  (is (nil? (classify-char "")))
  (is (= :numeric (classify-char "0")))
  (is (nil? (classify-char "01"))))

(deftest test-is-valid-number
  (testing "valid"
    (doseq [s ["2" "0089" "-0.1" "+3.14" "4." "-.9" "2e10" "-90E3"
               "3e+7" "+6e-1" "53.5e93" "-123.456e789"]]
      (is (true? (is-valid-number s)) s)))
  (testing "invalid"
    (doseq [s ["abc" "1a" "1e" "e3" "99e2.5" "--6" "-+3" "95a54e53" "."]]
      (is (false? (is-valid-number s)) s))))
