(ns clojure-algorithms.matrix.count-islands-in-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.count-islands-in-matrix :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-Matrix))
  (is (some? #'sut/Matrix-is-safe))
  (is (some? #'sut/Matrix-diffs))
  (is (some? #'sut/Matrix-count-islands)))
