(ns clojure-algorithms.project-euler.problem-027.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-027.sol1 :refer [prime? solution]]))

(deftest prime?-test
  (is (true? (prime? 2)))
  (is (true? (prime? 3)))
  (is (false? (prime? 27)))
  (is (true? (prime? 2999)))
  (is (false? (prime? 0)))
  (is (false? (prime? 1)))
  (is (false? (prime? -10))))

(deftest solution-test
  (is (= -4925 (solution 200 200)))
  (is (= 0 (solution -1000 1000)))
  (is (= 0 (solution -1000 -1000))))
