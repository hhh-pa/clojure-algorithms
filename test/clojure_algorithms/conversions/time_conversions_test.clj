(ns clojure-algorithms.conversions.time-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.time-conversions :refer [convert-time]]))

(deftest test-convert-time
  (is (= 1.0 (convert-time 3600 "seconds" "hours")))
  (is (= 0.972 (convert-time 3500 "Seconds" "Hours")))
  (is (= 24.0 (convert-time 1 "DaYs" "hours")))
  (is (= 7200.0 (convert-time 120 "minutes" "SeCoNdS")))
  (is (= 14.0 (convert-time 2 "WEEKS" "days")))
  (is (= 30.0 (convert-time 0.5 "hours" "MINUTES")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-time -3600 "seconds" "hours")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-time "Hello" "hours" "minutes")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-time [0 1 2] "weeks" "days")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-time 1 "cool" "century")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-time 1 "seconds" "hot"))))
