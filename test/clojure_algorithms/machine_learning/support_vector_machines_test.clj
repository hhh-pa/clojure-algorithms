(ns clojure-algorithms.machine-learning.support-vector-machines-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.support-vector-machines :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/norm-squared))
  (is (some? #'sut/make-SVC))
  (is (some? #'sut/SVC-linear))
  (is (some? #'sut/SVC-rbf))
  (is (some? #'sut/SVC-fit))
  (is (some? #'sut/SVC-predict)))
