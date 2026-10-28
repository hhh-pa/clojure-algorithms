(ns clojure-algorithms.maths.zellers-congruence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.zellers-congruence :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/zeller)))
