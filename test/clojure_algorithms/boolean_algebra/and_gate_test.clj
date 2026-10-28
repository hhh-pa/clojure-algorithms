(ns clojure-algorithms.boolean-algebra.and-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.and-gate :as g]))
(deftest and-gate-test
  (is (= 0 (g/and-gate 0 0)))
  (is (= 0 (g/and-gate 0 1)))
  (is (= 0 (g/and-gate 1 0)))
  (is (= 1 (g/and-gate 1 1)))
  (is (thrown? IllegalArgumentException (g/and-gate 2 1)))
  (is (thrown? IllegalArgumentException (g/and-gate 0 "1"))))
(deftest n-input-test
  (is (= 0 (g/n-input-and-gate [1 0 1 1 0])))
  (is (= 1 (g/n-input-and-gate [1 1 1 1 1])))
  (is (thrown? IllegalArgumentException (g/n-input-and-gate [])))
  (is (thrown? IllegalArgumentException (g/n-input-and-gate [1 2 1])))
  (is (thrown? IllegalArgumentException (g/n-input-and-gate [1 "1"]))))
