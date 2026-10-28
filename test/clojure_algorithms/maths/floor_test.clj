(ns clojure-algorithms.maths.floor-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.floor :as sut]))

(deftest test-fn-floor-examples
  (is (true? (every? true?
                     (map (fn [n] (= (sut/floor n) (long (Math/floor (double n)))))
                          [1 -1 0 1.1 -1.1 1.0 -1.0 1000000000])))))

(deftest test-fn-floor-values
  (is (= 1 (sut/floor 1)))
  (is (= -1 (sut/floor -1)))
  (is (= 0 (sut/floor 0)))
  (is (= 1 (sut/floor 1.1)))
  (is (= -2 (sut/floor -1.1)))
  (is (= 1 (sut/floor 1.0)))
  (is (= -1 (sut/floor -1.0)))
  (is (= 1000000000 (sut/floor 1000000000)))
  (is (= 2 (sut/floor 2.99)))
  (is (= -3 (sut/floor -2.01))))
