(ns clojure-algorithms.cellular-automata.langtons-ant-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.langtons-ant :as l]))
(deftest init-test
  (let [a (l/make-ant 2 2)]
    (is (= [[true true] [true true]] (:board a)))
    (is (= [1 1] (:pos a)))))
(deftest move-test
  (let [a (l/move-ant (l/make-ant 2 2))]
    (is (= [[true true] [true false]] (:board a)))
    (is (= [[true false] [true false]]
           (:board (l/move-ant a))))))
