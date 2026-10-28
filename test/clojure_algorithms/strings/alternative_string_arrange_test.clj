(ns clojure-algorithms.strings.alternative-string-arrange-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.alternative-string-arrange
             :refer [alternative-string-arrange]]))

(deftest test-alternative-string-arrange
  (testing "doctest examples"
    (is (= "AXBYCD" (alternative-string-arrange "ABCD" "XY")))
    (is (= "XAYBCD" (alternative-string-arrange "XY" "ABCD")))
    (is (= "AXBYZ" (alternative-string-arrange "AB" "XYZ")))
    (is (= "ABC" (alternative-string-arrange "ABC" ""))))
  (testing "edges"
    (is (= "" (alternative-string-arrange "" "")))
    (is (= "XYZ" (alternative-string-arrange "" "XYZ")))))
