(ns clojure-algorithms.project-euler.problem-032.sol32-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-032.sol32 :refer [combination-valid?]]))

(deftest combination-valid?-test
  (is (true? (combination-valid? [\3 \9 \1 \8 \6 \7 \2 \5 \4])))
  (is (false? (combination-valid? [\1 \2 \3 \4 \5 \6 \7 \8 \9]))))
