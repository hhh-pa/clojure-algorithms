(ns clojure-algorithms.web-programming.world-covid19-stats-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.world-covid19-stats
             :refer [stats-map format-report world-covid19-stats
                     default-url]]))

(deftest test-pure-logic
  (is (= {"Coronavirus Cases" "100" "Deaths" "2"}
         (stats-map [" Coronavirus Cases " " Deaths "] [" 100 " " 2 "])))
  (is (= "A\n1\nB\n2"
         (format-report (array-map "A" "1" "B" "2"))))
  (is (= "https://www.worldometers.info/coronavirus/" default-url)))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (world-covid19-stats))))
