(ns clojure-algorithms.maths.mobius-function-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.mobius-function :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/mobius)))
