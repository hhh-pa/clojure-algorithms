(ns clojure-algorithms.maths.print-multiplication-table-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.print-multiplication-table :as sut]))

(deftest test-fn-multiplication-table-examples
  (is (= "3 * 1 = 3\n3 * 2 = 6\n3 * 3 = 9\n3 * 4 = 12\n3 * 5 = 15"
         (sut/multiplication-table 3 5)))
  (is (= "-4 * 1 = -4\n-4 * 2 = -8\n-4 * 3 = -12\n-4 * 4 = -16\n-4 * 5 = -20\n-4 * 6 = -24"
         (sut/multiplication-table -4 6))))
