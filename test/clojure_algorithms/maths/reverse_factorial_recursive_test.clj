(ns clojure-algorithms.maths.reverse-factorial-recursive-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.reverse-factorial-recursive :as sut]))

(deftest test-fn-reverse-factorial-recursive-examples
  (is (approx 5 (sut/reverse-factorial-recursive 120)))
  (is (approx 4 (sut/reverse-factorial-recursive 24)))
  (is (approx -1 (sut/reverse-factorial-recursive 150)))
  (is (approx 1 (sut/reverse-factorial-recursive 1)))
  (is (approx 2 (sut/reverse-factorial-recursive 2))))
