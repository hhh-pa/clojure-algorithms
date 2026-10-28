(ns clojure-algorithms.maths.series.logarithmic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.series.logarithmic-series :as sut]))

(deftest test-fn-logarithmic-series-examples
  (is (approx ["(2^1)/1" "-(2^2)/2" "(2^3)/3" "-(2^4)/4" "(2^5)/5"] (sut/logarithmic-series 3)))
  (is (approx ["-(4^1)/1" "(4^2)/2" "-(4^3)/3" "(4^4)/4" "-(4^5)/5"] (sut/logarithmic-series -3)))
  (is (approx ["(2^1)/1" "-(2^2)/2" "(2^3)/3" "-(2^4)/4" "(2^5)/5" "-(2^6)/6"] (sut/logarithmic-series 3 6)))
  (is (approx [2.0 -2.0 2.6666666666666665 -4.0 6.4] (sut/logarithmic-series 3 5 true))))
