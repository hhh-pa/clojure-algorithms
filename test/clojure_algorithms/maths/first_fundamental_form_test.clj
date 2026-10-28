(ns clojure-algorithms.maths.first-fundamental-form-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.first-fundamental-form :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/first-fundamental-form)))
