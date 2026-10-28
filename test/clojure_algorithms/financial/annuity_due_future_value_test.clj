(ns clojure-algorithms.financial.annuity-due-future-value-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.annuity-due-future-value :refer [annuity-due-future-value]]))

(defn- round2 [x] (/ (Math/round (* 100.0 (double x))) 100.0))

(deftest annuity-due-future-value-test
  (is (= 6603.39 (round2 (annuity-due-future-value 500 10 0.05))))
  (is (= 13206.79 (round2 (annuity-due-future-value 1000 10 0.05))))
  (is (= 17531.17 (round2 (annuity-due-future-value 1000 10 0.10))))
  (is (= 63002.5 (round2 (annuity-due-future-value 1000 20 0.10))))
  (is (> (annuity-due-future-value 1000 3 0.05) (* 3 1000))))
