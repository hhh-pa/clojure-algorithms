(ns clojure-algorithms.financial.straight-line-depreciation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.straight-line-depreciation
             :refer [straight-line-depreciation]]))

(deftest depreciation-examples
  (is (= (vec (repeat 10 100.0)) (straight-line-depreciation 10 1100.0 100.0)))
  (is (= (vec (repeat 6 200.0)) (straight-line-depreciation 6 1250.0 50.0)))
  (is (= [250.25 250.25 250.25 250.25] (straight-line-depreciation 4 1001.0)))
  (is (= [4885.0] (straight-line-depreciation 1 4985 100)))
  (is (= 1000.0 (reduce + (straight-line-depreciation 10 1100.0 100.0)))))

(deftest depreciation-errors
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Useful years cannot be less than 1"
                        (straight-line-depreciation 0 100.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Purchase value cannot be less than zero"
                        (straight-line-depreciation 5 -10.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cannot be less than residual value"
                        (straight-line-depreciation 5 10.0 20.0))))
