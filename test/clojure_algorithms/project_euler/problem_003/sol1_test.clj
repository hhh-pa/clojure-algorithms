(ns clojure-algorithms.project-euler.problem-003.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-003.sol1 :refer [prime? solution]]))

(deftest prime?-test
  (is (true? (prime? 2)))
  (is (true? (prime? 3)))
  (is (false? (prime? 27)))
  (is (true? (prime? 2999)))
  (is (false? (prime? 0)))
  (is (false? (prime? 1))))

(deftest solution-test
  (is (= 29 (solution 13195)))
  (is (= 5 (solution 10)))
  (is (= 17 (solution 17)))
  (is (= 3 (solution 3.4)))
  (is (thrown? clojure.lang.ExceptionInfo (solution 0)))
  (is (thrown? clojure.lang.ExceptionInfo (solution -17)))
  (is (thrown? clojure.lang.ExceptionInfo (solution [])))
  (is (thrown? clojure.lang.ExceptionInfo (solution "asd"))))
