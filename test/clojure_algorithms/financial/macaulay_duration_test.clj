(ns clojure-algorithms.financial.macaulay-duration-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.macaulay-duration :refer [macaulay-duration]]))

(defn- round2 [x] (/ (Math/round (* 100.0 (double x))) 100.0))

(deftest macaulay-examples
  (is (= 6.83 (round2 (macaulay-duration 1000.0 0.05 8 0.04))))
  (is (= 4.43 (round2 (macaulay-duration 987435.34 0.07 5 0.038))))
  (is (= 5.62 (round2 (macaulay-duration 3564.2 0.023 6 0.071)))))

(deftest macaulay-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"face_value must be > 0"
                        (macaulay-duration -1000.0 0.05 8 0.04)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"coupon_rate must be >= 0"
                        (macaulay-duration 1000.0 -0.05 8 0.04)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"periods must be > 0"
                        (macaulay-duration 1000.0 0.05 0 0.04)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"yield_rate must be > 0"
                        (macaulay-duration 1000.0 0.05 8 -0.04))))
