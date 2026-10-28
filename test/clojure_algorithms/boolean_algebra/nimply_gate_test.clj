(ns clojure-algorithms.boolean-algebra.nimply-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.nimply-gate :as g]))
(deftest nimply-test
  (is (= 0 (g/nimply-gate 0 0)))
  (is (= 0 (g/nimply-gate 0 1)))
  (is (= 1 (g/nimply-gate 1 0)))
  (is (= 0 (g/nimply-gate 1 1))))
