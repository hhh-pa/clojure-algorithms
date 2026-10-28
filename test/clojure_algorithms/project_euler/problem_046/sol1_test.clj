(ns clojure-algorithms.project-euler.problem-046.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-046.sol1 :refer [prime? compute-nums solution]]))

(deftest prime?-test
  (is (false? (prime? 27)))
  (is (true? (prime? 563)))
  (is (false? (prime? 67483))))

(deftest compute-nums-test
  (is (= [5777] (compute-nums 1)))
  (is (= [5777 5993] (compute-nums 2)))
  (is (thrown? IllegalArgumentException (compute-nums 0)))
  (is (thrown? IllegalArgumentException (compute-nums "a")))
  (is (thrown? IllegalArgumentException (compute-nums 1.1))))

(deftest solution-test
  (is (= 5777 (solution))))
