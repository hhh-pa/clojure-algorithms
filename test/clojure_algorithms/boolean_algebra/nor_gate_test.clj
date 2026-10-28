(ns clojure-algorithms.boolean-algebra.nor-gate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure-algorithms.boolean-algebra.nor-gate :as g]))
(deftest nor-test
  (is (= 1 (g/nor-gate 0 0)))
  (is (= 0 (g/nor-gate 0 1)))
  (is (= 0 (g/nor-gate 1 0)))
  (is (= 0 (g/nor-gate 1 1)))
  (is (= 0 (g/nor-gate 0 -7))))
(deftest table-test
  (let [t (g/truth-table g/nor-gate)]
    (is (str/includes? t "Truth Table of NOR Gate:"))
    (is (= 6 (count (str/split-lines t))))))
