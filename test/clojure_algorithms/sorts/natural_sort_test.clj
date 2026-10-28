(ns clojure-algorithms.sorts.natural-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.natural-sort :refer [natural-sort]]))

(deftest test-natural-sort
  (testing "doctest examples"
    (is (= ["1 ft 5 in" "2 ft 7 in" "2 ft 11 in" "7 ft 6 in" "10 ft 2 in"]
           (natural-sort ["2 ft 7 in" "1 ft 5 in" "10 ft 2 in"
                          "2 ft 11 in" "7 ft 6 in"])))
    (is (= ["elm0" "elm1" "Elm2" "elm9" "elm10" "Elm11" "Elm12" "elm13"]
           (natural-sort ["Elm11" "Elm12" "Elm2" "elm0"
                          "elm1" "elm10" "elm13" "elm9"]))))
  (testing "edges"
    (is (= [] (natural-sort [])))
    (is (= ["file1" "file2" "file10"]
           (natural-sort ["file10" "file2" "file1"])))))
