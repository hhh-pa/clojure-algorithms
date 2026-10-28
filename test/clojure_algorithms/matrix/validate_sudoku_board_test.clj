(ns clojure-algorithms.matrix.validate-sudoku-board-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.validate-sudoku-board :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(deftest test-fn-is-valid-sudoku-board-examples
  (is (= true (boolean (sut/is-valid-sudoku-board [["5" "3" "." "." "7" "." "." "." "."] ["6" "." "." "1" "9" "5" "." "." "."] ["." "9" "8" "." "." "." "." "6" "."] ["8" "." "." "." "6" "." "." "." "3"] ["4" "." "." "8" "." "3" "." "." "1"] ["7" "." "." "." "2" "." "." "." "6"] ["." "6" "." "." "." "." "2" "8" "."] ["." "." "." "4" "1" "9" "." "." "5"] ["." "." "." "." "8" "." "." "7" "9"]]))))
  (is (= true (boolean (sut/is-valid-sudoku-board [["1" "2" "3" "4" "5" "6" "7" "8" "9"] ["4" "5" "6" "7" "8" "9" "1" "2" "3"] ["7" "8" "9" "1" "2" "3" "4" "5" "6"] ["." "." "." "." "." "." "." "." "."] ["." "." "." "." "." "." "." "." "."] ["." "." "." "." "." "." "." "." "."] ["." "." "." "." "." "." "." "." "."] ["." "." "." "." "." "." "." "." "."] ["." "." "." "." "." "." "." "." "."]]))))
  (is (= true (boolean (sut/is-valid-sudoku-board [["1" "2" "3" "." "." "." "." "." "."] ["4" "5" "6" "." "." "." "." "." "."] ["7" "8" "9" "." "." "." "." "." "."] ["." "." "." "4" "5" "6" "." "." "."] ["." "." "." "7" "8" "9" "." "." "."] ["." "." "." "1" "2" "3" "." "." "."] ["." "." "." "." "." "." "7" "8" "9"] ["." "." "." "." "." "." "1" "2" "3"] ["." "." "." "." "." "." "4" "5" "6"]]))))
  (is (= true (boolean (sut/is-valid-sudoku-board [["1" "2" "3" "." "." "." "5" "6" "4"] ["4" "5" "6" "." "." "." "8" "9" "7"] ["7" "8" "9" "." "." "." "2" "3" "1"] ["." "." "." "4" "5" "6" "." "." "."] ["." "." "." "7" "8" "9" "." "." "."] ["." "." "." "1" "2" "3" "." "." "."] ["3" "1" "2" "." "." "." "7" "8" "9"] ["6" "4" "5" "." "." "." "1" "2" "3"] ["9" "7" "8" "." "." "." "4" "5" "6"]]))))
  (is (= true (boolean (sut/is-valid-sudoku-board [["1" "2" "3" "8" "9" "7" "5" "6" "4"] ["4" "5" "6" "2" "3" "1" "8" "9" "7"] ["7" "8" "9" "5" "6" "4" "2" "3" "1"] ["2" "3" "1" "4" "5" "6" "9" "7" "8"] ["5" "6" "4" "7" "8" "9" "3" "1" "2"] ["8" "9" "7" "1" "2" "3" "6" "4" "5"] ["3" "1" "2" "6" "4" "5" "7" "8" "9"] ["6" "4" "5" "9" "7" "8" "1" "2" "3"] ["9" "7" "8" "3" "1" "2" "4" "5" "6"]]))))
  (is (thrown? Exception (sut/is-valid-sudoku-board [["1" "2" "3" "4" "5" "6" "7" "8" "9"]])))
  (is (thrown? Exception (sut/is-valid-sudoku-board [["1"] ["2"] ["3"] ["4"] ["5"] ["6"] ["7"] ["8"] ["9"]]))))
