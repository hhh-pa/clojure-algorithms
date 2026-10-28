(ns clojure-algorithms.blockchain.diophantine-equation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.blockchain.diophantine-equation
             :refer [diophantine diophantine-all-solutions extended-gcd
                     all-diophantine-solutions greatest-common-divisor]]))

(deftest extended-gcd-test
  (is (= [2 -1 2] (extended-gcd 10 6)))
  (is (= [1 -2 3] (extended-gcd 7 5)))
  (is (thrown? clojure.lang.ExceptionInfo (extended-gcd -1 5))))

(deftest diophantine-test
  (is (= [-7.0 14.0] (diophantine 10 6 14)))
  (is (= [9.0 -12.0] (diophantine 391 299 -69)))
  (is (thrown? clojure.lang.ExceptionInfo (diophantine 3 6 10))))

(deftest diophantine-all-solutions-test
  (is (= [[-7.0 14.0] [-4.0 9.0]] (diophantine-all-solutions 10 6 14)))
  (is (= [[-7.0 14.0] [-4.0 9.0] [-1.0 4.0] [2.0 -1.0]]
         (diophantine-all-solutions 10 6 14 4)))
  (is (= [[9.0 -12.0] [22.0 -29.0] [35.0 -46.0] [48.0 -63.0]]
         (diophantine-all-solutions 391 299 -69 4))))

(deftest all-diophantine-solutions-test
  (is (= [[-7 14] [-4 9]] (all-diophantine-solutions 10 6 14 2)))
  (is (= [[-7 14] [-4 9] [-1 4] [2 -1]] (all-diophantine-solutions 10 6 14 4)))
  (is (thrown? clojure.lang.ExceptionInfo (all-diophantine-solutions 3 6 10 1)))
  (testing "every returned pair satisfies the equation"
    (doseq [[x y] (all-diophantine-solutions 391 299 -69 6)]
      (is (= -69 (+ (* 391 x) (* 299 y))))))
  (testing "a = b = 0 edge cases"
    (is (= [[0 0]] (all-diophantine-solutions 0 0 0 2)))
    (is (thrown? clojure.lang.ExceptionInfo (all-diophantine-solutions 0 0 5 1)))))

(deftest greatest-common-divisor-test
  (is (= 2 (greatest-common-divisor 10 6)))
  (is (= 1 (greatest-common-divisor 7 5))))
