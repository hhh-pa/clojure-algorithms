(ns clojure-algorithms.maths.fast-inverse-sqrt-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.fast-inverse-sqrt :as sut]))

(deftest test-fn-fast-inverse-sqrt-examples
  (is (approx 0.3156857923527257 (sut/fast-inverse-sqrt 10)))
  (is (approx 0.49915357479239103 (sut/fast-inverse-sqrt 4)))
  (is (approx 0.4932849504615651 (sut/fast-inverse-sqrt 4.1)))
  (is (thrown? Exception (sut/fast-inverse-sqrt 0)))
  (is (thrown? Exception (sut/fast-inverse-sqrt -1))))
