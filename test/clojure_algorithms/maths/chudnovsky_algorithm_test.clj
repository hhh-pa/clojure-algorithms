(ns clojure-algorithms.maths.chudnovsky-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.chudnovsky-algorithm :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/pi)))
