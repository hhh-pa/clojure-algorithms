(ns clojure-algorithms.financial.equated-monthly-installments-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.equated-monthly-installments
             :refer [equated-monthly-installments]]))

(deftest emi-examples
  (is (< (Math/abs (- (equated-monthly-installments 25000 0.12 3) 830.3577453212793)) 1e-9))
  (is (< (Math/abs (- (equated-monthly-installments 25000 0.12 10) 358.67737100646826)) 1e-9)))

(deftest emi-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Principal borrowed must be > 0"
                        (equated-monthly-installments 0 0.12 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Rate of interest must be >= 0"
                        (equated-monthly-installments 25000 -1 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Years to repay must be an integer > 0"
                        (equated-monthly-installments 25000 0.12 0))))
