(ns clojure-algorithms.maths.hardy-ramanujanalgo-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.hardy-ramanujanalgo :as sut]))

(deftest test-fn-exact-prime-factor-count-examples
  (is (approx 3 (sut/exact-prime-factor-count 51242183))))
