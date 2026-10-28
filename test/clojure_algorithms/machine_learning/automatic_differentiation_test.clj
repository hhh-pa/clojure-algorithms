(ns clojure-algorithms.machine-learning.automatic-differentiation-test
  (:require [clojure-algorithms.machine-learning.automatic-differentiation
             :as sut]
            [clojure.test :refer [deftest is testing]]))

(deftest test-op-type-enum
  (is (= {:ADD 0 :SUB 1 :MUL 2 :DIV 3 :MATMUL 4 :POWER 5 :NOOP 6} sut/OpType))
  (is (= [0 1 2 3 4 5 6]
         [sut/OpType-ADD sut/OpType-SUB sut/OpType-MUL sut/OpType-DIV
          sut/OpType-MATMUL sut/OpType-POWER sut/OpType-NOOP]))
  (is (= :ADD (sut/make-OpType 0)))
  (is (= :NOOP (sut/make-OpType 6)))
  (is (= :ADD (sut/make-OpType :ADD))))

(deftest test-operation-nodes
  (let [op (sut/make-Operation sut/OpType-ADD)]
    (is (= sut/OpType-ADD (:op-type op)))
    (is (= {} (:other-params op)))
    (is (= {"power" 3}
           (:other-params (sut/make-Operation sut/OpType-POWER {"power" 3})))))
  (testing "params/output attach purely"
    (let [op (-> (sut/make-Operation sut/OpType-MUL)
                 (sut/Operation-add-params [:a :b])
                 (sut/Operation-add-output :out))]
      (is (= [:a :b] (:params op)))
      (is (= :out (:output op)))))
  (testing "equality compares op-type ids only"
    (let [op (sut/make-Operation sut/OpType-DIV)]
      (is (true? (sut/Operation-eq op sut/OpType-DIV)))
      (is (false? (sut/Operation-eq op sut/OpType-ADD)))
      (is (false? (sut/Operation-eq op :DIV))))))

(deftest test-gradient-tracker-singleton
  (testing "construction resets the shared state"
    (let [t1 (sut/make-GradientTracker)]
      (sut/GradientTracker-enter t1)
      (is (true? (:enabled @t1)))
      (let [t2 (sut/make-GradientTracker)]
        (is (identical? t1 t2))
        (is (false? (:enabled @t2))))))
  (testing "exit disables"
    (let [tracker (sut/make-GradientTracker)]
      (sut/GradientTracker-enter tracker)
      (is (nil? (sut/GradientTracker-exit tracker nil nil nil)))
      (is (false? (:enabled @tracker))))))

(deftest test-gradient-tracker-append
  (testing "append wires params and output through the operation"
    (let [tracker (sut/make-GradientTracker)
          a (atom {:value 2.0 :param-to [] :result-of nil})
          b (atom {:value 3.0 :param-to [] :result-of nil})
          out (atom {:value nil :param-to [] :result-of nil})]
      (is (nil? (sut/GradientTracker-append tracker sut/OpType-ADD [a b] out)))
      (is (= 1 (count (:param-to @a))))
      (is (= 1 (count (:param-to @b))))
      (is (= sut/OpType-ADD (:op-type (:result-of @out))))
      (is (= [a b] (:params (:result-of @out))))
      (is (= out (:output (:result-of @out)))))))

(deftest test-variable-repr
  (is (= "Variable(5.0)" (sut/Variable-repr {:value 5.0})))
  (is (= "Variable(5.0)" (sut/Variable-repr (atom {:value 5.0})))))

(deftest test-ndarray-stubs
  (doseq [[f args] [[sut/make-Variable [[5.0]]]
                    [sut/Variable-to-ndarray [nil]]
                    [sut/Variable-add [nil nil]]
                    [sut/Variable-sub [nil nil]]
                    [sut/Variable-mul [nil nil]]
                    [sut/Variable-truediv [nil nil]]
                    [sut/Variable-matmul [nil nil]]
                    [sut/Variable-pow [nil 2]]
                    [sut/GradientTracker-gradient [nil nil nil]]
                    [sut/GradientTracker-derivative [nil nil nil]]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
