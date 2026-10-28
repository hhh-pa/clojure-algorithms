(ns clojure-algorithms.other.doomsday-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.doomsday :as d]))

(deftest get-week-day-test
  (testing "doctest examples"
    (is (= "Saturday" (d/get-week-day 2020 10 24)))
    (is (= "Tuesday" (d/get-week-day 2017 10 24)))
    (is (= "Friday" (d/get-week-day 2019 5 3)))
    (is (= "Wednesday" (d/get-week-day 1970 9 16)))
    (is (= "Saturday" (d/get-week-day 1870 8 13)))
    (is (= "Wednesday" (d/get-week-day 2040 3 14)))))
