(ns clojure-algorithms.maths.reverse-factorial-recursive-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.reverse-factorial-recursive :as sut]))

(deftest test-fn-reverse-factorial-recursive-examples
  (is (approx 5 (sut/reverse-factorial-recursive 120)))
  (is (approx 4 (sut/reverse-factorial-recursive 24)))
  (is (approx -1 (sut/reverse-factorial-recursive 150)))
  (is (approx 1 (sut/reverse-factorial-recursive 1)))
  (is (approx 2 (sut/reverse-factorial-recursive 2)))
  (is (approx 6 (sut/reverse-factorial-recursive 720)))
  (is (thrown? Exception (sut/reverse-factorial-recursive 1.5)))
  (is (thrown? Exception (sut/reverse-factorial-recursive 120 1.5)))
  (is (thrown? Exception (sut/reverse-factorial-recursive 0)))
  (is (thrown? Exception (sut/reverse-factorial-recursive -5))))
