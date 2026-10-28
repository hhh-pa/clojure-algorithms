(ns clojure-algorithms.financial.expected-shortfall-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.expected-shortfall :refer [expected-shortfall]]))

(deftest expected-shortfall-examples
  (is (= 10.0 (expected-shortfall [-10 -5 -2 1 4] 0.95)))
  (is (= 7.5 (expected-shortfall [-10 -5 -2 1 4] 0.75))))

(deftest expected-shortfall-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"returns must not be empty"
                        (expected-shortfall [] 0.95)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"confidence_level must be strictly between 0 and 1"
                        (expected-shortfall [-1 0 1] 0.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"returns must contain only finite numbers"
                        (expected-shortfall [-1 Double/POSITIVE_INFINITY 1] 0.95))))
