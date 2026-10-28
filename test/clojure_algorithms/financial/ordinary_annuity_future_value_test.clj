(ns clojure-algorithms.financial.ordinary-annuity-future-value-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.ordinary-annuity-future-value
             :refer [ordinary-annuity-future-value]]))

(defn- round2 [x] (/ (Math/round (* 100.0 (double x))) 100.0))

(deftest ordinary-annuity-test
  (is (= 6288.95 (round2 (ordinary-annuity-future-value 500 10 0.05))))
  (is (= 12577.89 (round2 (ordinary-annuity-future-value 1000 10 0.05))))
  (is (= 15937.42 (round2 (ordinary-annuity-future-value 1000 10 0.10))))
  (is (= 57275.0 (round2 (ordinary-annuity-future-value 1000 20 0.10)))))
