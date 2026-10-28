(ns clojure-algorithms.boolean-algebra.xor-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.xor-gate :as g]))
(deftest xor-test
  (is (= 0 (g/xor-gate 0 0)))
  (is (= 1 (g/xor-gate 0 1)))
  (is (= 1 (g/xor-gate 1 0)))
  (is (= 0 (g/xor-gate 1 1))))
