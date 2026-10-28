(ns clojure-algorithms.machine-learning.decision-tree-test
  (:require [clojure-algorithms.machine-learning.decision-tree :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest test-fn-mean-squared-error-examples
  (let [tester (sut/make-DecisionTree)]
    (is (= (sut/DecisionTree-mean-squared-error tester [1 2 3 4 5 6 7 8 9 10]
                                                (double 6))
           (sut/TestDecisionTree-helper-mean-squared-error-test
            [1 2 3 4 5 6 7 8 9 10] (double 6))))
    (is (= (sut/DecisionTree-mean-squared-error tester [1 2 3] (double 2))
           (sut/TestDecisionTree-helper-mean-squared-error-test
            [1 2 3] (double 2))))
    (is (approx 8.5 (sut/DecisionTree-mean-squared-error
                     tester [1 2 3 4 5 6 7 8 9 10] (double 6))))))

(deftest test-fn-train-examples
  (testing "1-D inputs of equal length train without errors"
    (let [tree (sut/DecisionTree-train (sut/make-DecisionTree)
                                       [10 20 30 40 50] [0 0 0 1 1])]
      (is (approx 0.4 (:prediction tree)))))
  (is (thrown? Exception (sut/DecisionTree-train (sut/make-DecisionTree)
                                                 [[1 2 3 4 5] [1 2 3 4 5]]
                                                 [0 0 0 1 1])))
  (is (thrown? Exception (sut/DecisionTree-train (sut/make-DecisionTree)
                                                 [1 2 3 4 5]
                                                 [[0 0 0 1 1] [0 0 0 1 1]])))
  (is (thrown? Exception (sut/DecisionTree-train (sut/make-DecisionTree)
                                                 [1 2 3 4 5]
                                                 [[1] [2] [3] [4] [5]])))
  (testing "error types and precedence"
    (is (= :ValueError
           (:type (try (sut/DecisionTree-train (sut/make-DecisionTree)
                                               [[1 2]] [[3 4] [5 6]])
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (= "x and y have different lengths"
           (try (sut/DecisionTree-train (sut/make-DecisionTree) [1 2 3]
                                        [[0 0 0 1 1] [0 0 0 1 1]])
                (catch clojure.lang.ExceptionInfo e (.getMessage e)))))
    (is (= "Data set labels must be one-dimensional"
           (try (sut/DecisionTree-train (sut/make-DecisionTree) [1 2 3]
                                        [[1] [2] [3]])
                (catch clojure.lang.ExceptionInfo e (.getMessage e)))))))

(deftest test-fn-predict-examples
  (testing "train then predict on separable data"
    (let [tree (sut/DecisionTree-train (sut/make-DecisionTree 5 2)
                                       [1 2 3 4 5 6 7 8]
                                       [0 0 0 0 1 1 1 1])]
      (is (approx 0.0 (sut/DecisionTree-predict tree 2.0)))
      (is (approx 1.0 (sut/DecisionTree-predict tree 7.0)))))
  (testing "untrained tree raises"
    (is (= :ValueError
           (:type (try (sut/DecisionTree-predict (sut/make-DecisionTree) 1.0)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest test-fn-main-smoke
  (let [out (with-out-str (sut/main))]
    (is (str/includes? out "Test values: "))
    (is (str/includes? out "Predictions: "))
    (is (str/includes? out "Average error: "))))
