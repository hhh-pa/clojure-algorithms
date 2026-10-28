(ns clojure-algorithms.maths.ceil-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.ceil :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/ceil)))
