(ns clojure-algorithms.data-structures.arrays.sudoku-solver-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.sudoku-solver
             :refer [cross squares unitlist units peers grid-values parse-grid
                     solved? solve display-grid shuffled random-puzzle solve-all
                     grid1 grid2]]))

(deftest test-cross
  (is (= ["A1" "A2" "B1" "B2"] (cross "AB" "12")))
  (is (= ["A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3"]
         (cross "ABC" "123")))
  (is (= [] (cross "" "12")))
  (is (= [] (cross "A" "")))
  (is (= [] (cross "" ""))))

(deftest test-structure
  (is (= 81 (count squares)))
  (is (= 27 (count unitlist)))
  (is (every? #(= 3 (count (units %))) squares))
  (is (every? #(= 20 (count (peers %))) squares))
  (is (= [["A2" "B2" "C2" "D2" "E2" "F2" "G2" "H2" "I2"]
          ["C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"]
          ["A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3"]]
         (units "C2")))
  (is (= #{"A2" "B2" "D2" "E2" "F2" "G2" "H2" "I2" "C1" "C3" "C4" "C5" "C6"
           "C7" "C8" "C9" "A1" "A3" "B1" "B3"}
         (peers "C2"))))

(deftest test-solve-easy
  (let [values (solve grid2)]
    (is (not (false? values)))
    (is (solved? values))
    (doseq [[s d] (grid-values grid2)
            :when (not (#{\0 \.} d))]
      (is (= (str d) (values s)))))
  (let [values (solve grid1)]
    (is (not (false? values)))
    (is (solved? values))))

(deftest test-contradiction
  (is (false? (parse-grid (apply str (repeat 81 \1))))))

(deftest test-helpers
  (is (= 81 (count (grid-values grid1))))
  (is (string? (display-grid (parse-grid grid1))))
  (is (= (sort squares) (sort (shuffled squares))))
  (let [p (random-puzzle)]
    (is (= 81 (count p))))
  (is (= 2 (:total (solve-all [grid1 grid2])))))
