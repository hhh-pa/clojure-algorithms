(ns clojure-algorithms.strings.min-cost-string-conversion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.min-cost-string-conversion
             :refer [compute-transform-tables assemble-transformation]]))

(deftest test-compute-transform-tables
  (let [[costs ops] (compute-transform-tables "cat" "cut" 1 2 3 3)]
    (is (= [0 3 6 9] (subvec (nth costs 0) 0 4)))
    (is (= [6 4 3 6] (subvec (nth costs 2) 0 4)))
    (is (= ["0" "Ic" "Iu" "It"] (subvec (nth ops 0) 0 4)))
    (is (= ["Dt" "Dt" "Rtu" "Ct"] (subvec (nth ops 3) 0 4))))
  (is (= [[[0]] [["0"]]] (compute-transform-tables "" "" 1 2 3 3))))

(deftest test-assemble-transformation
  (let [ops [["0" "Ic" "Iu" "It"]
             ["Dc" "Cc" "Iu" "It"]
             ["Da" "Da" "Rau" "Rat"]
             ["Dt" "Dt" "Rtu" "Ct"]]
        x (dec (count ops))
        y (dec (count (first ops)))]
    (is (= ["Cc" "Rau" "Ct"] (assemble-transformation ops x y)))
    (is (= ["Cc" "Da" "Rtu"] (assemble-transformation ops x (dec y)))))
  (is (= [] (assemble-transformation [["0"]] 0 0)))
  (let [ops2 [["0" "I1" "I2" "I3"]
              ["D1" "C1" "I2" "I3"]
              ["D2" "D2" "R23" "R23"]]]
    (is (= ["C1" "I2" "R23"]
           (assemble-transformation ops2 2 3)))))
