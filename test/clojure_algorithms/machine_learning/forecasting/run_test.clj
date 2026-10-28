(ns clojure-algorithms.machine-learning.forecasting.run-test
  (:require [clojure-algorithms.machine-learning.forecasting.run :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-linear-regression-prediction-examples
  (let [n (sut/linear-regression-prediction [2 3 4 5] [5 3 4 6] [3 1 2 4]
                                            [2 1] [2 2])]
    (is (< (Math/abs (- n 5.0)) 1e-6)))
  (testing "exact linear data recovers the plane (up to the raw-event quirk)"
    ;; y = 1 + date + event; forecast is |1 + 5 + 2 + 1|
    (is (approx 9.0 (sut/linear-regression-prediction [1 2 3 4]
                                                      [4 4 8 8]
                                                      [2 1 4 3]
                                                      [5] [2])))))

(deftest test-fn-interquartile-range-checker-examples
  (is (approx 2.8 (sut/interquartile-range-checker [1 2 3 4 5 6 7 8 9 10])))
  (testing "order of inputs does not matter"
    (is (approx 2.8 (sut/interquartile-range-checker
                     [10 1 9 2 8 3 7 4 6 5])))))

(deftest test-fn-data-safety-checker-examples
  (is (false? (sut/data-safety-checker [2 3 4] 5.0)))
  (is (true? (sut/data-safety-checker [5.0 5.05 4.95] 5.0)))
  (is (= :TypeError (:type (try (sut/data-safety-checker [2 3 4] 5)
                                (catch clojure.lang.ExceptionInfo e
                                  (ex-data e)))))))

(deftest test-fn-stubs
  (doseq [[f args] [[sut/sarimax-predictor [[4 2 6 8] [3 1 2 4] [2]]]
                    [sut/support-vector-regressor [[[5 2] [1 5] [6 2]]
                                                   [[3 2]] [2 1 4]]]
                    [sut/random-forest-regressor [[[5 2] [1 5] [6 2]]
                                                  [[3 2]] [2 1 4]]]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
