(ns clojure-algorithms.maths.line-length-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.line-length :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/line-length)))
