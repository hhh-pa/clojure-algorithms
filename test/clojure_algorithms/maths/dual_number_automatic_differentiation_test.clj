(ns clojure-algorithms.maths.dual-number-automatic-differentiation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.dual-number-automatic-differentiation :as sut]))

(deftest test-fn-differentiate-examples
  (is (approx 2 (sut/differentiate #(sut/Dual-pow % 2) 2 2)))
  (is (approx 196830
              (sut/differentiate #(sut/Dual-mul (sut/Dual-pow % 2)
                                                (sut/Dual-pow % 4))
                                 9 2)))
  (is (approx 7605.0
              (sut/differentiate #(sut/Dual-mul (sut/Dual-pow (sut/Dual-add % 3) 6)
                                                0.5)
                                 3.5 4)))
  (is (approx 0 (sut/differentiate #(sut/Dual-pow % 2) 4 3)))
  (is (thrown? Exception (sut/differentiate 8 8 8)))
  (is (thrown? Exception (sut/differentiate #(sut/Dual-pow % 2) "" 1)))
  (is (thrown? Exception (sut/differentiate #(sut/Dual-pow % 2) 3 ""))))

(deftest test-dual-arithmetic
  (testing "construction and repr"
    (is (= "3+1E1" (sut/Dual-repr (sut/make-Dual 3 1))))
    (is (= [1 1] (:duals (sut/make-Dual 0 2)))))
  (testing "add"
    (is (approx [5 [3 2]]
                (let [d (sut/Dual-add (sut/make-Dual 2 [1 1])
                                      (sut/make-Dual 3 [2 1]))]
                  [(:real d) (:duals d)])))
    (is (approx [7 [1]] (let [d (sut/Dual-add (sut/make-Dual 2 1) 5)]
                          [(:real d) (:duals d)]))))
  (testing "sub"
    (is (approx [1 [0]] (let [d (sut/Dual-sub (sut/make-Dual 3 [2])
                                              (sut/make-Dual 2 [2]))]
                          [(:real d) (:duals d)]))))
  (testing "mul"
    (is (approx [6 [5]] (let [d (sut/Dual-mul (sut/make-Dual 2 [1])
                                              (sut/make-Dual 3 [1]))]
                          [(:real d) (subvec (:duals d) 0 1)]))))
  (testing "truediv and floordiv by scalar"
    (is (approx [2.0 [0.5]] (let [d (sut/Dual-truediv (sut/make-Dual 4 [1]) 2)]
                              [(:real d) (:duals d)])))
    (is (approx [2 [0]] (let [d (sut/Dual-floordiv (sut/make-Dual 4 [1]) 2)]
                          [(:real d) (:duals d)])))
    (is (thrown? Exception (sut/Dual-truediv (sut/make-Dual 1 1)
                                             (sut/make-Dual 1 1))))
    (is (thrown? Exception (sut/Dual-floordiv (sut/make-Dual 1 1)
                                              (sut/make-Dual 1 1)))))
  (testing "pow"
    (is (approx 1 (sut/Dual-pow (sut/make-Dual 3 1) 0)))
    (is (thrown? Exception (sut/Dual-pow (sut/make-Dual 3 1) -1)))
    (is (thrown? Exception (sut/Dual-pow (sut/make-Dual 3 1) 1.5))))
  (testing "reduce drops trailing zeroes"
    (is (= [1 2] (:duals (sut/Dual-reduce (sut/make-Dual 9 [1 2 0 0]))))))
  (testing "reflected operator aliases"
    (is (approx [7 [1]] (let [d (sut/Dual---radd-- (sut/make-Dual 2 1) 5)]
                          [(:real d) (:duals d)])))
    (is (approx [6 [3]] (let [d (sut/Dual---rmul-- (sut/make-Dual 2 [1]) 3)]
                          [(:real d) (:duals d)])))))
