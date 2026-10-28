(ns clojure-algorithms.project-euler.problem-203.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-203.sol1 :refer [get-pascal-triangle-unique-coefficients
                                                                       get-squarefrees
                                                                       solution]]))

(deftest get-pascal-triangle-unique-coefficients-test
  (is (= #{1} (get-pascal-triangle-unique-coefficients 1)))
  (is (= #{1} (get-pascal-triangle-unique-coefficients 2)))
  (is (= #{1 2} (get-pascal-triangle-unique-coefficients 3)))
  (is (= #{1 2 3 4 5 6 7 35 10 15 20 21}
         (get-pascal-triangle-unique-coefficients 8))))

(deftest get-squarefrees-test
  (is (= #{1} (get-squarefrees #{1})))
  (is (= #{1 2} (get-squarefrees #{1 2})))
  (is (= #{1 2 3 5 6 7 35 10 15 21}
         (get-squarefrees #{1 2 3 4 5 6 7 35 10 15 20 21}))))

(deftest solution-test
  (is (= 1 (solution 1)))
  (is (= 105 (solution 8)))
  (is (= 175 (solution 9))))
