(ns clojure-algorithms.maths.continued-fraction-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.continued-fraction :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/continued-fraction)))
