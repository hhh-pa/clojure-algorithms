(ns clojure-algorithms.maths.sock-merchant-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.sock-merchant :as sut]))

(deftest test-fn-sock-merchant-examples
  (is (approx 3 (sut/sock-merchant [10 20 20 10 10 30 50 10 20])))
  (is (approx 2 (sut/sock-merchant [1 1 3 3]))))

(deftest test-fn-sock-merchant-extra
  (is (approx 0 (sut/sock-merchant [])))
  (is (approx 0 (sut/sock-merchant [1 2 3])))
  (is (approx 2 (sut/sock-merchant [5 5 5 5 5]))))
