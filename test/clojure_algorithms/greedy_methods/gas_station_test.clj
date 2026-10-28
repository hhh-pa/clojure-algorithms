(ns clojure-algorithms.greedy-methods.gas-station-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.gas-station
             :refer [->GasStation can-complete-journey get-gas-stations]]))

(deftest test-get-gas-stations
  (let [stations (get-gas-stations [1 2 3 4 5] [3 4 5 1 2])]
    (is (= 5 (count stations)))
    (is (= (->GasStation 1 3) (first stations)))
    (is (= (->GasStation 5 2) (last stations)))))

(deftest test-can-complete-journey
  (is (= 3 (can-complete-journey (get-gas-stations [1 2 3 4 5] [3 4 5 1 2]))))
  (is (= -1 (can-complete-journey (get-gas-stations [2 3 4] [3 4 3])))))
