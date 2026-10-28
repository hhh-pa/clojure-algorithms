(ns clojure-algorithms.boolean-algebra.nand-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.nand-gate :as g]))
(deftest nand-test
  (is (= 1 (g/nand-gate 0 0)))
  (is (= 1 (g/nand-gate 0 1)))
  (is (= 1 (g/nand-gate 1 0)))
  (is (= 0 (g/nand-gate 1 1))))
