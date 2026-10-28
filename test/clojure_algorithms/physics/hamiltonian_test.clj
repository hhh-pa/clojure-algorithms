(ns clojure-algorithms.physics.hamiltonian-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.hamiltonian
             :refer [classical-hamiltonian quantum-hamiltonian]]))

(deftest test-classical-hamiltonian
  (testing "known values"
    (is (= 14.0 (classical-hamiltonian 2 3 5)))
    (is (= 10.0 (classical-hamiltonian 1 0 10)))
    (is (= 16.0 (classical-hamiltonian 2 -4 0))))
  (testing "non-positive mass throws"
    (is (thrown? clojure.lang.ExceptionInfo (classical-hamiltonian -1 2 5)))))

(deftest test-quantum-hamiltonian
  (testing "known matrices"
    (is (= [[1.0 -0.5 0.0] [-0.5 1.0 -0.5] [0.0 -0.5 1.0]]
           (quantum-hamiltonian 3 [0.0 0.0 0.0])))
    (is (= [[1.5 -0.25] [-0.25 2.5]]
           (quantum-hamiltonian 2 [1.0 2.0] :mass 2.0 :hbar 1.0 :dx 1.0))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (quantum-hamiltonian 3 [0.0 0.0])))
    (is (thrown? clojure.lang.ExceptionInfo (quantum-hamiltonian 0 [])))))
