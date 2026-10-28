(ns clojure-algorithms.matrix.max-area-of-island-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.max-area-of-island :as sut]))

(deftest test-fn-is-safe-examples
  (is (= true (boolean (sut/is-safe 0 0 5 5))))
  (is (= false (boolean (sut/is-safe -1 -1 5 5)))))

(deftest test-fn-depth-first-search-examples
  (is (approx 0 (sut/depth-first-search 0 0 #{} sut/matrix)))
  (is (approx 1 (sut/depth-first-search 0 2 #{} sut/matrix))))

(deftest test-fn-find-max-area-examples
  (is (approx 6 (sut/find-max-area sut/matrix)))
  (is (approx 0 (sut/find-max-area [[0 0] [0 0]]))))
