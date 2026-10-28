(ns clojure-algorithms.boolean-algebra.xnor-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.xnor-gate :as g]))
(deftest xnor-test
  (is (= 1 (g/xnor-gate 0 0)))
  (is (= 0 (g/xnor-gate 0 1)))
  (is (= 0 (g/xnor-gate 1 0)))
  (is (= 1 (g/xnor-gate 1 1))))
