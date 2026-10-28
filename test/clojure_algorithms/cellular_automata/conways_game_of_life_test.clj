(ns clojure-algorithms.cellular-automata.conways-game-of-life-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.conways-game-of-life :as c]))
(deftest blinker-test
  (is (= [[0 0 0] [1 1 1] [0 0 0]] (c/new-generation c/blinker)))
  (is (= c/blinker (c/new-generation (c/new-generation c/blinker)))))
(deftest still-test
  (is (= [[1 1] [1 1]] (c/new-generation [[1 1] [1 1]])))
  (is (= [[0 0] [0 0]] (c/new-generation [[0 0] [0 0]]))))
(deftest generations-test
  (is (= 3 (count (take 3 (c/generations c/blinker))))))
