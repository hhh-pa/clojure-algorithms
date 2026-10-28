(ns clojure-algorithms.project-euler.problem-037.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-037.sol1 :refer [prime? list-truncated-nums validate compute-truncated-primes]]))

(deftest prime?-test
  (is (false? (prime? 27)))
  (is (true? (prime? 563)))
  (is (false? (prime? 67483))))

(deftest list-truncated-nums-test
  (is (= [927628 27628 92762 7628 9276 628 927 28 92 8 9]
         (list-truncated-nums 927628)))
  (is (= [467 67 46 7 4] (list-truncated-nums 467)))
  (is (= [58 8 5] (list-truncated-nums 58))))

(deftest validate-test
  (is (false? (validate 74679)))
  (is (false? (validate 235693)))
  (is (true? (validate 3797))))

(deftest compute-truncated-primes-test
  (is (= [23 37] (compute-truncated-primes 2))))
