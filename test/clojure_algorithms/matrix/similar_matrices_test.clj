(ns clojure-algorithms.matrix.similar-matrices-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.similar-matrices :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/-as-square-matrix))
  (is (some? #'sut/-jordan-signature))
  (is (some? #'sut/are-similar-matrices))
  (is (some? #'sut/--all--))
  (is (some? #'sut/MatrixLike)))
