(ns clojure-algorithms.machine-learning.gradient-descent-test
  (:require [clojure-algorithms.machine-learning.gradient-descent :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]))

(use-fixtures :each (fn [t]
                      (reset! sut/parameter-vector [2 4 1 5])
                      (t)
                      (reset! sut/parameter-vector [2 4 1 5])))

(deftest test-fn-error-examples
  (is (= 24 (sut/-error 0))))

(deftest test-fn-hypothesis-value-examples
  (is (= 39 (sut/-hypothesis-value [5 2 3]))))

(deftest test-fn-output-examples
  (is (= 15 (sut/output 0 "train")))
  (is (= 150 (sut/output 1 "test")))
  (is (nil? (sut/output 0 "unknown"))))

(deftest test-fn-calculate-hypothesis-value-examples
  (is (= 39 (sut/calculate-hypothesis-value 0 "train")))
  (is (= 2149 (sut/calculate-hypothesis-value 0 "test")))
  (is (nil? (sut/calculate-hypothesis-value 0 "unknown"))))

(deftest test-fn-summation-of-cost-derivative-examples
  (is (= 243 (sut/summation-of-cost-derivative -1)))
  (is (= 2234 (sut/summation-of-cost-derivative 0)))
  (testing "end limits the summation range"
    (is (= 24 (sut/summation-of-cost-derivative -1 1)))))

(deftest test-fn-get-cost-derivative-examples
  (is (approx 48.6 (sut/get-cost-derivative -1)))
  (is (approx 446.8 (sut/get-cost-derivative 0))))

(deftest test-fn-run-gradient-descent-smoke
  (testing "descent converges and improves the fit"
    (let [sse (fn [] (reduce + (map #(let [e (sut/-error %)] (* e e))
                                    (range sut/m))))
          before (sse)
          out (with-out-str (sut/run-gradient-descent))
          after (sse)]
      (is (str/includes? out "Number of iterations:"))
      (is (< after before))
      (is (< (double after) 1.0)))))

(deftest test-fn-test-gradient-descent-smoke
  (let [out (with-out-str (sut/test-gradient-descent))]
    (is (str/includes? out "Actual output value:"))
    (is (str/includes? out "Hypothesis output:"))))
