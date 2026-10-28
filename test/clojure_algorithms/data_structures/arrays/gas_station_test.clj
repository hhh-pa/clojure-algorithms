(ns clojure-algorithms.data-structures.arrays.gas-station-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.gas-station
             :refer [make-gas-station can-complete-circuit]]))

(deftest test-can-complete-circuit
  (let [gs make-gas-station]
    (is (= 3 (can-complete-circuit [(gs 1 3) (gs 2 4) (gs 3 5) (gs 4 1) (gs 5 2)])))
    (is (= -1 (can-complete-circuit [(gs 2 3) (gs 3 4) (gs 4 3)])))
    (is (= 4 (can-complete-circuit [(gs 5 4) (gs 1 4) (gs 2 1) (gs 3 5) (gs 4 1)])))
    (is (= 0 (can-complete-circuit [(gs 5 4)])))))
