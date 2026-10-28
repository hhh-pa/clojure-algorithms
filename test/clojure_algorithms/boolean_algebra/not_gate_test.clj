(ns clojure-algorithms.boolean-algebra.not-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.not-gate :as g]))
(deftest not-test
  (is (= 1 (g/not-gate 0)))
  (is (= 0 (g/not-gate 1))))
