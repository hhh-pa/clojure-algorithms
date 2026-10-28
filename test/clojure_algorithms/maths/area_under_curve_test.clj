(ns clojure-algorithms.maths.area-under-curve-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.area-under-curve :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/trapezoidal-area)))
