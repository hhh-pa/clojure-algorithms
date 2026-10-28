(ns clojure-algorithms.cellular-automata.one-dimensional-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.one-dimensional :as o]))
(deftest ruleset-test
  (is (= [0 0 0 1 1 1 0 0] (o/format-ruleset 11100)))
  (is (= [0 0 0 0 0 0 0 0] (o/format-ruleset 0)))
  (is (= [1 1 1 1 1 1 1 1] (o/format-ruleset 11111111))))
(deftest generation-test
  (let [cells [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]
        rule (o/format-ruleset 30)]
    (is (= 31 (count (o/new-generation cells rule 0))))
    (is (= [1 0 1] (o/new-generation [[1 1 1]] [0 1 1 1 1 0 0 0] 0)))))
