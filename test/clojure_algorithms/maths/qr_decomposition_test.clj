(ns clojure-algorithms.maths.qr-decomposition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.qr-decomposition :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/qr-householder)))
