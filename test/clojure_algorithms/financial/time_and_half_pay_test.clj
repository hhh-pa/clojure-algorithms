(ns clojure-algorithms.financial.time-and-half-pay-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.time-and-half-pay :refer [pay]]))

(deftest pay-examples
  (is (= 41.5 (pay 41 1)))
  (is (= 1472.5 (pay 65 19)))
  (is (= 10.0 (pay 10 1)))
  (is (= 40.0 (pay 40 1)))
  (is (= 47.5 (pay 45 1 40))))
