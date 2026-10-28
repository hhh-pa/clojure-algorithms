(ns clojure-algorithms.web-programming.covid-stats-via-xpath-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.covid-stats-via-xpath
             :refer [covid-data-from-strings unavailable covid-stats
                     xpath-str]]))

(deftest test-pure-logic
  (let [d (covid-data-from-strings ["1" "2" "3"])]
    (is (= ["1" "2" "3"] [(:cases d) (:deaths d) (:recovered d)])))
  (is (= unavailable (covid-data-from-strings ["only-one"])))
  (is (= unavailable (covid-data-from-strings [])))
  (is (string? xpath-str)))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (covid-stats))))
