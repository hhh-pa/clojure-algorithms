(ns clojure-algorithms.maths.print-multiplication-table-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.print-multiplication-table :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/multiplication-table)))
