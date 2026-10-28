(ns clojure-algorithms.graphs.markov-chain-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.markov-chain
             :refer [make-graph add-transition-probability transition
                     get-transitions]]))

(def transitions
  [["a" "a" 0.9] ["a" "b" 0.075] ["a" "c" 0.025] ["b" "a" 0.15]
   ["b" "b" 0.8] ["b" "c" 0.05] ["c" "a" 0.25] ["c" "b" 0.25]
   ["c" "c" 0.5]])

(deftest test-markov-chain
  (let [g (add-transition-probability (make-graph) "x" "y" 1.0)]
    (is (= "y" (transition g "x")))
    (is (= "" (transition g "y"))))
  (let [result (get-transitions "a" transitions 5000)]
    (is (> (get result "a") (get result "b") (get result "c")))))
