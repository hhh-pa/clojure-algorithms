(ns clojure-algorithms.linear-algebra.src.conjugate-gradient-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.conjugate-gradient
             :refer [conjugate-gradient matrix-spd?]]))

(deftest test-matrix-spd?
  (is (true? (matrix-spd? [[4.12401784 -5.01453636 -0.63865857]
                           [-5.01453636 12.33347422 -3.40493586]
                           [-0.63865857 -3.40493586 5.78591885]])))
  (is (false? (matrix-spd? [[0.34634879 1.96165514 2.18277744]
                            [0.74074469 -1.19648894 -1.34223498]
                            [-0.7687067 0.06018373 -1.16315631]])))
  (is (false? (matrix-spd? [[1 2] [2 1]]))))

(deftest test-conjugate-gradient
  (let [a [[8.73256573 -5.02034289 -2.68709226]
           [-5.02034289 3.78188322 0.91980451]
           [-2.68709226 0.91980451 1.94746467]]
        b [-5.80872761 3.23807431 1.95381422]
        x (conjugate-gradient a b)
        expected [-0.63114139 -0.01561498 0.13979294]]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) x expected))))
  (let [x (conjugate-gradient [[4 1] [1 3]] [1 2])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) x [1/11 7/11])))))
