(ns clojure-algorithms.project-euler.problem-049.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-049.sol1 :refer [prime? search]]))

(deftest prime?-test
  (is (false? (prime? 27)))
  (is (true? (prime? 563)))
  (is (false? (prime? 67483))))

(deftest search-test
  (is (true? (search 3 [1 2 3])))
  (is (false? (search 4 [1 2 3])))
  (is (false? (search 101 (vec (range -100 100))))))
