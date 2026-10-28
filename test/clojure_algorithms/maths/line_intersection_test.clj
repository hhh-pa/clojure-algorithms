(ns clojure-algorithms.maths.line-intersection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.line-intersection :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/determinant))
  (is (some? #'sut/line-coefficients))
  (is (some? #'sut/segment-intersection)))
