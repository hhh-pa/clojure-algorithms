(ns clojure-algorithms.maths.eulers-totient-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.eulers-totient :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/totient)))
