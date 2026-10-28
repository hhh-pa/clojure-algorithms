(ns clojure-algorithms.conversions.molecular-chemistry-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.molecular-chemistry
             :refer [molarity-to-normality moles-to-pressure moles-to-volume
                     pressure-and-volume-to-temperature mass-to-moles
                     moles-to-molecules]]))

(deftest test-molecular-chemistry
  (is (= 20 (molarity-to-normality 2 3.1 0.31)))
  (is (= 8 (molarity-to-normality 4 11.4 5.7)))
  (is (= 90 (moles-to-pressure 0.82 3 300)))
  (is (= 10 (moles-to-pressure 8.2 5 200)))
  (is (= 90 (moles-to-volume 0.82 3 300)))
  (is (= 10 (moles-to-volume 8.2 5 200)))
  (is (= 20 (pressure-and-volume-to-temperature 0.82 1 2)))
  (is (= 60 (pressure-and-volume-to-temperature 8.2 5 3)))
  (is (= 2.0 (mass-to-moles 36.03 18.015)))
  (is (= 0.25 (mass-to-moles 11.0 44.01)))
  (is (thrown? clojure.lang.ExceptionInfo (mass-to-moles 10 0)))
  (is (thrown? clojure.lang.ExceptionInfo (mass-to-moles -1 10)))
  (is (= 1.2044e+24 (moles-to-molecules 2)))
  (is (= 3.011e+23 (moles-to-molecules 0.5)))
  (is (thrown? clojure.lang.ExceptionInfo (moles-to-molecules -1))))
