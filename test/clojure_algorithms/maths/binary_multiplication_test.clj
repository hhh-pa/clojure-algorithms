(ns clojure-algorithms.maths.binary-multiplication-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.binary-multiplication :as sut]))

(deftest test-fn-binary-multiply-examples
  (is (approx 6 (sut/binary-multiply 2 3)))
  (is (approx 0 (sut/binary-multiply 5 0)))
  (is (approx 12 (sut/binary-multiply 3 4)))
  (is (approx 50 (sut/binary-multiply 10 5)))
  (is (approx 0 (sut/binary-multiply 0 5)))
  (is (approx 2 (sut/binary-multiply 2 1)))
  (is (approx 10 (sut/binary-multiply 1 10))))

(deftest test-fn-binary-mod-multiply-examples
  (is (approx 1 (sut/binary-mod-multiply 2 3 5)))
  (is (approx 0 (sut/binary-mod-multiply 5 0 7)))
  (is (approx 0 (sut/binary-mod-multiply 3 4 6)))
  (is (approx 11 (sut/binary-mod-multiply 10 5 13)))
  (is (approx 2 (sut/binary-mod-multiply 2 1 5)))
  (is (approx 1 (sut/binary-mod-multiply 1 10 3))))
