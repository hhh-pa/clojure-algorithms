(ns clojure-algorithms.matrix.spiral-print-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.spiral-print :as sut]))

(deftest test-fn-spiral-print-clockwise-examples
  (is (= "1\n2\n3\n4\n8\n12\n11\n10\n9\n5\n6\n7\n" (with-out-str (sut/spiral-print-clockwise [[1 2 3 4] [5 6 7 8] [9 10 11 12]])))))
