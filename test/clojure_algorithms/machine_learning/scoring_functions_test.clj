(ns clojure-algorithms.machine-learning.scoring-functions-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.scoring-functions :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/mae))
  (is (some? #'sut/mse))
  (is (some? #'sut/rmse))
  (is (some? #'sut/rmsle))
  (is (some? #'sut/mbd))
  (is (some? #'sut/manual-accuracy)))
