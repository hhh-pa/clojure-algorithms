(ns clojure-algorithms.maths.special-numbers.hexagonal-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.hexagonal-number :as sut]))

(deftest test-fn-hexagonal-examples
  (is (approx 28 (sut/hexagonal 4)))
  (is (approx 231 (sut/hexagonal 11)))
  (is (approx 946 (sut/hexagonal 22)))
  (is (thrown? Exception (sut/hexagonal 0)))
  (is (thrown? Exception (sut/hexagonal -1)))
  (is (thrown? Exception (sut/hexagonal 11.0))))
