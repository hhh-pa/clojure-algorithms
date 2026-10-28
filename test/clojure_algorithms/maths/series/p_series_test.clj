(ns clojure-algorithms.maths.series.p-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.p-series :as sut]))

(deftest test-fn-p-series-examples
  (is (approx ["1" "1 / 4" "1 / 9" "1 / 16" "1 / 25"] (sut/p-series 5 2)))
  (is (approx [] (sut/p-series -5 2)))
  (is (approx ["1" "1 / 0.25" "1 / 0.1111111111111111" "1 / 0.0625" "1 / 0.04"] (sut/p-series 5 -2)))
  (is (approx [""] (sut/p-series "" 1000)))
  (is (approx [] (sut/p-series 0 0)))
  (is (approx ["1"] (sut/p-series 1 1))))
