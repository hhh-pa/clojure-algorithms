(ns clojure-algorithms.maths.series.hexagonal-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.hexagonal-numbers :as sut]))

(deftest test-fn-hexagonal-numbers-examples
  (is (approx [0 1 6 15 28 45 66 91 120 153] (sut/hexagonal-numbers 10)))
  (is (approx [0 1 6 15 28] (sut/hexagonal-numbers 5)))
  (is (thrown? Exception (sut/hexagonal-numbers 0))))
