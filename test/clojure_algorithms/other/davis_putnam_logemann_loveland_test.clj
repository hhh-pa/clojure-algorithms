(ns clojure-algorithms.other.davis-putnam-logemann-loveland-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.davis-putnam-logemann-loveland :as d]))

(deftest clause-evaluate-test
  (is (true? (d/clause-evaluate ["A1" "A2'" "A3"] {"A1" true})))
  (is (nil? (d/clause-evaluate ["A1" "A2"] {})))
  (is (false? (d/clause-evaluate ["A1"] {"A1" false})))
  (testing "tautology is true"
    (is (true? (d/clause-evaluate ["A1" "A1'"] {})))))

(deftest generate-parameters-test
  (let [[clauses symbols]
        (d/generate-parameters [["A1" "A2'" "A3"] ["A5'" "A2'" "A1"]])]
    (is (= [["A1" "A2'" "A3"] ["A5'" "A2'" "A1"]] clauses))
    (is (= ["A1" "A2" "A3" "A5"] symbols))))

(deftest find-pure-symbols-test
  (let [[clauses symbols]
        (d/generate-parameters [["A1" "A2'" "A3"] ["A5'" "A2'" "A1"]])
        [pure assign] (d/find-pure-symbols clauses symbols {})]
    (is (= ["A1" "A2" "A3" "A5"] pure))
    (is (= {"A1" true "A2" false "A3" true "A5" false} assign))))

(deftest find-unit-clauses-test
  (let [[clauses _]
        (d/generate-parameters [["A4" "A3" "A5'" "A1" "A3'"] ["A4"] ["A3"]])
        [units assign] (d/find-unit-clauses clauses {})]
    (is (= ["A4" "A3"] units))
    (is (= {"A4" true "A3" true} assign))))

(deftest dpll-test
  (testing "doctest example"
    (let [[clauses symbols]
          (d/generate-parameters [["A4" "A3" "A5'" "A1" "A3'"] ["A4"]])
          [sat model] (d/dpll clauses symbols {})]
      (is (true? sat))
      (is (= {"A4" true} model))))
  (testing "unsatisfiable formula"
    (is (= [false nil] (d/solve [["A1"] ["A1'"]]))))
  (testing "satisfiable formula validates"
    (let [formula [["A1" "A2"] ["A1'" "A3"] ["A2'" "A3'"]]
          [sat model] (d/solve formula)]
      (is (true? sat))
      (is (every? true? (map #(d/clause-evaluate % model) formula))))))
