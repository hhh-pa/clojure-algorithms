(ns clojure-algorithms.financial.interest-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.interest
             :refer [simple-interest compound-interest apr-interest]]))

(defn- approx= [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest simple-interest-test
  (is (= 3240.0 (simple-interest 18000.0 0.06 3)))
  (is (approx= 0.09 (simple-interest 0.5 0.06 3)))
  (is (= 0.0 (simple-interest 18000.0 0.0 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"daily_interest_rate must be >= 0"
                        (simple-interest 10000.0 -0.06 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"principal must be > 0"
                        (simple-interest -10000.0 0.06 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"days_between_payments must be > 0"
                        (simple-interest 5500.0 0.01 -5))))

(deftest compound-interest-test
  (is (approx= 1576.25 (compound-interest 10000.0 0.05 3)))
  (is (approx= 500.0 (compound-interest 10000.0 0.05 1)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"number_of_compounding_periods must be > 0"
                        (compound-interest 10000.0 0.06 -4)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nominal_annual_interest_rate_percentage must be >= 0"
                        (compound-interest 10000.0 -3.5 3.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"principal must be > 0"
                        (compound-interest -5500.0 0.01 5))))

(deftest apr-interest-test
  (is (approx= 1618.223072263547 (apr-interest 10000.0 0.05 3)))
  (is (approx= 512.6749646744732 (apr-interest 10000.0 0.05 1)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"number_of_years must be > 0"
                        (apr-interest 10000.0 0.06 -4)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nominal_annual_percentage_rate must be >= 0"
                        (apr-interest 10000.0 -3.5 3.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"principal must be > 0"
                        (apr-interest -5500.0 0.01 5))))
