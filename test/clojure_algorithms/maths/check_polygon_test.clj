(ns clojure-algorithms.maths.check-polygon-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.check-polygon :as sut]))

(deftest test-fn-check-polygon-examples
  (is (= true (boolean (sut/check-polygon [6 10 5]))))
  (is (= false (boolean (sut/check-polygon [3 7 13 2]))))
  (is (= false (boolean (sut/check-polygon [1 4.3 5.2 12.2]))))
  (is (approx [3 7 13 2] (let [nums [3 7 13 2] --v (sut/check-polygon nums)] nums)))
  (is (thrown? Exception (sut/check-polygon [])))
  (is (thrown? Exception (sut/check-polygon [4 5])))
  (is (thrown? Exception (sut/check-polygon [-2 5 6]))))
