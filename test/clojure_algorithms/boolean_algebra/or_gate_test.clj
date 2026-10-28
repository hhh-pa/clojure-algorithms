(ns clojure-algorithms.boolean-algebra.or-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.or-gate :as g]))
(deftest or-test
  (is (= 0 (g/or-gate 0 0)))
  (is (= 1 (g/or-gate 0 1)))
  (is (= 1 (g/or-gate 1 0)))
  (is (= 1 (g/or-gate 1 1))))
