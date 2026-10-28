(ns clojure-algorithms.machine-learning.linear-regression-test
  (:require [clojure-algorithms.machine-learning.linear-regression :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest test-fn-run-steep-gradient-descent-examples
  (is (approx [0.196 0.343]
              (sut/run-steep-gradient-descent [[1 2] [3 4]] [5 6] 2 0.01
                                              [0.1 0.2]))))

(deftest test-fn-sum-of-square-error-examples
  (is (approx 0.03
              (sut/sum-of-square-error [[1.1] [2.1] [3.1]] [1.2 2.2 3.2] [1])))
  (testing "perfect fit has zero error"
    (is (approx 0.0 (sut/sum-of-square-error [[1.0] [2.0]] [2.0 4.0] [2.0])))))

(deftest test-fn-run-linear-regression-smoke
  (testing "100k-step fit converges toward the least-squares solution"
    (let [result (atom nil)
          out (with-out-str
                (reset! result (sut/run-linear-regression [[1 1] [1 2] [1 3]]
                                                          [3 5 7])))]
      (is (str/includes? out "At Iteration 1 - Error is"))
      (is (str/includes? out "At Iteration 100000 - Error is"))
      (let [[theta err] @result]
        (is (= [] err))
        ;; y = 2x + 1 with bias column: theta settles near [1 2]
        (is (< (Math/abs (- (first theta) 1.0)) 0.05))
        (is (< (Math/abs (- (second theta) 2.0)) 0.05))
        (is (< (sut/sum-of-square-error [[1 1] [1 2] [1 3]] [3 5 7] theta)
               1e-3))))))

(deftest test-fn-mean-absolute-error-examples
  (is (approx 0.5 (sut/mean-absolute-error [3 -0.5 2 7] [2.5 0.0 2 8])))
  (is (approx 0.0 (sut/mean-absolute-error [1.0 2.0] [1.0 2.0]))))

(deftest test-fn-stubs
  (doseq [[f args] [[sut/collect-dataset []]
                    [sut/plot-regression [nil nil nil]]
                    [sut/plot-loss [nil]]
                    [sut/main []]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
