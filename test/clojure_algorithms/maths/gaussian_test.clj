(ns clojure-algorithms.maths.gaussian-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.gaussian :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/gaussian)))
