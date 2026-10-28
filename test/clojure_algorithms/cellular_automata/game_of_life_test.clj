(ns clojure-algorithms.cellular-automata.game-of-life-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.game-of-life :as g]))
(deftest canvas-test
  (is (= [[false]] (g/create-canvas 1)))
  (is (= [] (g/create-canvas 0)))
  (is (= 3 (count (g/create-canvas 3)))))
(deftest judge-test
  (is (true? (g/judge-point true [[true true false]
                                  [false true false]
                                  [false false false]])))
  (is (false? (g/judge-point true [[true false false]
                                   [false true false]
                                   [false false false]])))
  (is (true? (g/judge-point false [[true true false]
                                   [true false false]
                                   [false false false]])))
  (is (false? (g/judge-point false [[true false false]
                                    [false false false]
                                    [false false false]]))))
(deftest run-test
  (is (= [[false false false] [false false false] [false false false]]
         (g/run [[false false false] [false false false] [false false false]])))
  (let [blinker [[false false false false false]
                 [false false true false false]
                 [false false true false false]
                 [false false true false false]
                 [false false false false false]]]
    (is (= [false true true true false] (nth (g/run blinker) 2)))))
