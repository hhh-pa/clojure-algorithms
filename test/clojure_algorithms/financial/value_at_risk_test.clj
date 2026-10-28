(ns clojure-algorithms.financial.value-at-risk-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.value-at-risk :refer [value-at-risk]]))

(deftest var-examples
  (is (= 9.0 (value-at-risk [-10 -5 -2 1 4] 0.95)))
  (is (= 1.5 (value-at-risk [-2 -1 0 1 2 3] 0.90)))
  (is (= -7.5 (value-at-risk [5 10 15] 0.75))))

(deftest var-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"returns must not be empty"
                        (value-at-risk [] 0.95)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"confidence_level must be strictly between 0 and 1"
                        (value-at-risk [-1 0 1] 1.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"returns must contain only finite numbers"
                        (value-at-risk [-1 Double/NaN 1] 0.95))))
