(ns clojure-algorithms.boolean-algebra.imply-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.imply-gate :as g]))
(deftest imply-test
  (is (= 1 (g/imply-gate 0 0)))
  (is (= 1 (g/imply-gate 0 1)))
  (is (= 0 (g/imply-gate 1 0)))
  (is (= 1 (g/imply-gate 1 1))))
(deftest recursive-test
  (is (thrown? IllegalArgumentException (g/recursive-imply-list [])))
  (is (thrown? IllegalArgumentException (g/recursive-imply-list [0])))
  (is (= 1 (g/recursive-imply-list [0 0])))
  (is (= 0 (g/recursive-imply-list [1 0])))
  (is (= 0 (g/recursive-imply-list [0 0 0])))
  (is (= 1 (g/recursive-imply-list [1 0 1])))
  (is (= 0 (g/recursive-imply-list [1 1 0])))
  (is (= 1 (g/recursive-imply-list [1 1 1]))))
