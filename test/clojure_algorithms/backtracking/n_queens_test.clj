(ns clojure-algorithms.backtracking.n-queens-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.n-queens :refer [safe? solve printboard]]))

(deftest safe-test
  (is (true? (safe? [[0 0 0] [0 0 0] [0 0 0]] 1 1)))
  (is (false? (safe? [[0 1 0] [0 0 0] [0 0 0]] 1 1)))
  (is (false? (safe? [[1 0 0] [0 0 0] [0 0 0]] 1 1)))
  (is (false? (safe? [[0 0 1] [0 0 0] [0 0 0]] 1 1)))
  (is (true? (safe? [[1 0 0] [0 0 0] [0 0 0]] 1 2)))
  (is (true? (safe? [[1 0 0] [0 0 0] [0 0 0]] 2 1)))
  (is (true? (safe? [[0 0 0] [1 0 0] [0 0 0]] 0 2)))
  (is (true? (safe? [[0 0 0] [1 0 0] [0 0 0]] 2 2))))

(deftest solve-test
  (testing "known solution counts"
    (is (= 1 (count (solve [[0]] 0))))
    (is (= 0 (count (solve [[0 0] [0 0]] 0))))
    (is (= 2 (count (solve (vec (repeat 4 (vec (repeat 4 0)))) 0)))))
  (testing "every solution holds n queens, one per row and column"
    (doseq [board (solve (vec (repeat 4 (vec (repeat 4 0)))) 0)]
      (is (= 4 (count (filter #{1} (apply concat board)))))
      (doseq [row board]
        (is (= 1 (count (filter #{1} row))))))))
