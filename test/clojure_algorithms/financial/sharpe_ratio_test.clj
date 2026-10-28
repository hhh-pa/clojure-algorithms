(ns clojure-algorithms.financial.sharpe-ratio-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.sharpe-ratio
             :refer [sharpe-ratio annualized-sharpe-ratio]]))

(defn- round4 [x] (/ (Math/round (* 10000.0 (double x))) 10000.0))

(deftest sharpe-examples
  (is (= 2.2164 (round4 (sharpe-ratio [0.1 0.2 0.15 0.05 0.12]))))
  (is (= Double/POSITIVE_INFINITY (sharpe-ratio [0.05 0.05 0.05 0.05 0.05])))
  (is (= 1.8589 (round4 (sharpe-ratio [0.1 0.2 0.15 0.05 0.12] 0.02))))
  (is (= 0.0 (sharpe-ratio [0.0 0.0 0.0 0.0 0.0])))
  (is (= -2.6261 (round4 (sharpe-ratio [-0.05 -0.1 -0.08 -0.12 -0.15]))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"returns list must not be empty"
                        (sharpe-ratio [])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 2 values"
                        (sharpe-ratio [0.1]))))

(deftest annualized-examples
  (is (= 35.1844 (round4 (annualized-sharpe-ratio [0.001 0.002 0.0015 0.0005 0.0012] 0.0 252))))
  (is (= 7.6779 (round4 (annualized-sharpe-ratio [0.01 0.02 0.015 0.005 0.012] 0.0 12))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"periods_per_year must be > 0"
                        (annualized-sharpe-ratio [0.001 0.002 0.0015] 0.0 0))))
