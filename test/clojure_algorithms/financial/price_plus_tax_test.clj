(ns clojure-algorithms.financial.price-plus-tax-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.price-plus-tax :refer [price-plus-tax]]))

(deftest price-plus-tax-examples
  (is (= 125.0 (price-plus-tax 100 0.25)))
  (is (< (Math/abs (- (price-plus-tax 125.50 0.05) 131.775)) 1e-9))
  (is (= 100.0 (price-plus-tax 100 0.0))))
