(ns clojure-algorithms.web-programming.get-top-billionaires-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.get-top-billionaires
             :refer [years-old format-worth person-row format-table
                     get-forbes-real-time-billionaires]])
  (:import [java.time LocalDate LocalDateTime ZoneOffset]))

(defn- ts
  [y m d]
  (.toEpochSecond (.atZone (LocalDateTime/of y m d 0 0) ZoneOffset/UTC)))

(deftest test-years-old
  (let [today (LocalDate/of 2024 1 12)]
    (is (= 64 (years-old (ts 1959 11 20) today)))
    (is (= 53 (years-old (ts 1970 2 13) today)))
    (is (every? (fn [i] (= i (years-old (ts (- 2024 i) 1 12) today)))
                (range 1 111)))))

(deftest test-format-worth
  (is (= "211.0 Billion" (format-worth 211000))))

(deftest test-person-row
  (let [row (person-row {"personName" "X" "source" "Y" "countryOfCitizenship" "Z"
                         "gender" "M" "finalWorth" 100000 "birthDate" 0}
                        (LocalDate/of 2024 1 12))]
    (is (= "X" (get row "Name")))
    (is (= "100.0 Billion" (get row "Worth ($)")))
    (is (= "54" (get row "Age")))))

(deftest test-format-table
  (is (string? (format-table [{"Name" "X" "Age" "54"}]))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (get-forbes-real-time-billionaires))))
