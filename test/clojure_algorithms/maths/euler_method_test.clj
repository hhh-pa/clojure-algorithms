(ns clojure-algorithms.maths.euler-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.euler-method :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/explicit-euler)))
