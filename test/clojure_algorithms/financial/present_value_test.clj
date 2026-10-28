(ns clojure-algorithms.financial.present-value-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.present-value :refer [present-value]]))

(deftest present-value-examples
  (is (= 4.69 (present-value 0.13 [10 20.70 -293 297])))
  (is (= 175519.15 (present-value 0.07 [109129.39 30923.23 15098.93 29734 39])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Discount rate cannot be negative"
                        (present-value -1 [1 2])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cash flows list cannot be empty"
                        (present-value 0.03 []))))
