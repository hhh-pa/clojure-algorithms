(ns clojure-algorithms.project-euler.problem-112.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-112.sol1 :refer [check-bouncy
                                                                       solution]]))

(deftest check-bouncy-test
  (is (false? (check-bouncy 6789)))
  (is (false? (check-bouncy -12345)))
  (is (false? (check-bouncy 0)))
  (is (thrown? IllegalArgumentException (check-bouncy 6.74)))
  (is (true? (check-bouncy 132475)))
  (is (false? (check-bouncy 34)))
  (is (true? (check-bouncy 341)))
  (is (false? (check-bouncy 47)))
  (is (true? (check-bouncy -6548))))

(deftest solution-test
  (is (= 538 (solution 50)))
  (is (= 21780 (solution 90)))
  (is (= 4770 (solution 80)))
  (is (thrown? IllegalArgumentException (solution 105)))
  (is (thrown? IllegalArgumentException (solution 100.011))))
