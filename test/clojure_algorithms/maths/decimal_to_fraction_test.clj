(ns clojure-algorithms.maths.decimal-to-fraction-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.decimal-to-fraction :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/decimal-to-fraction)))
