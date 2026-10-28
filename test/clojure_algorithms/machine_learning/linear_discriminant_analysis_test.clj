(ns clojure-algorithms.machine-learning.linear-discriminant-analysis-test
  (:require [clojure-algorithms.machine-learning.linear-discriminant-analysis
             :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private gauss-5-1-20
  [6.288184753155463 6.4494456086997705 5.066335808938262 4.235456349028368
   3.9078267848958586 5.031334516831717 3.977896829989127 3.56317055489747
   5.199311976483754 5.133374604658605 5.546468300338232 4.086029056264687
   5.005005283626573 4.935258239627312 3.494170998739258 5.537997178661033
   5.320711100998849 7.3891120432406865 5.202969177309964 4.855297691835079])

(deftest test-fn-gaussian-distribution-examples
  (is (approx gauss-5-1-20 (sut/gaussian-distribution 5.0 1.0 20)))
  (testing "deterministic across calls and correctly sized"
    (is (= (sut/gaussian-distribution 5.0 1.0 20)
           (sut/gaussian-distribution 5.0 1.0 20)))
    (is (= 7 (count (sut/gaussian-distribution 0.0 2.0 7))))))

(deftest test-fn-y-generator-examples
  (is (= [0 0 0 0 0 0 0 0 0 0] (sut/y-generator 1 [10])))
  (is (= [0 0 0 0 0 1 1 1 1 1 1 1 1 1 1] (sut/y-generator 2 [5 10])))
  (is (= (vec (concat (repeat 10 0) (repeat 5 1) (repeat 15 2) (repeat 20 3)))
         (sut/y-generator 4 [10 5 15 20]))))

(deftest test-fn-calculate-mean-examples
  (is (approx 5.011267842911003
              (sut/calculate-mean 20 (sut/gaussian-distribution 5.0 1.0 20)))))

(deftest test-fn-calculate-probabilities-examples
  (is (approx 0.3333333333333333 (sut/calculate-probabilities 20 60)))
  (is (approx 0.3 (sut/calculate-probabilities 30 100))))

(deftest test-fn-calculate-variance-examples
  (is (approx 0.9618530973487491
              (sut/calculate-variance
               [(sut/gaussian-distribution 5.0 1.0 20)] [5.011267842911003] 20))))

(deftest test-fn-predict-y-values-examples
  (let [base (sut/gaussian-distribution 5.0 1.0 20)
        x-items [(vec base) (mapv #(+ % 5.0) base) (mapv #(+ % 10.0) base)]
        means [5.011267842911003 10.011267842911003 15.011267842911002]
        variance 0.9618530973487494
        probabilities [0.3333333333333333 0.3333333333333333 0.3333333333333333]]
    (is (= (vec (concat (repeat 20 0) (repeat 20 1) (repeat 20 2)))
           (sut/predict-y-values x-items means variance probabilities)))))

(deftest test-fn-accuracy-examples
  (is (approx 50.0
              (sut/accuracy [0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1]
                            [0 0 0 1 1 1 0 0 1 1 0 0 0 0 1 1 1 0 1 1 1])))
  (is (approx 100.0
              (sut/accuracy [0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2 2]
                            [0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2 2]))))

(deftest test-fn-valid-input-examples
  (testing "parses valid input"
    (is (= 5 (with-in-str "5\n" (sut/valid-input :int "n: " "bad" #(> % 0))))))
  (testing "empty line falls back to default"
    (is (= 1.0 (with-in-str "\n"
                 (sut/valid-input :float "s: " "bad" #(>= % 0) "1.0")))))
  (testing "bad input and failed conditions retry"
    (let [out (java.io.StringWriter.)]
      (binding [*out* out]
        (is (= 3 (with-in-str "abc\n-2\n3\n"
                   (sut/valid-input :int "n: " "must be positive" #(> % 0))))))
      (is (str/includes? (str out) "Incorrect input type"))
      (is (str/includes? (str out) "must be positive")))))

(deftest test-fn-main-smoke
  (testing "scripted demo run quits cleanly"
    (let [out (with-out-str
                (with-in-str "2\n1.0\n5\n5\n0.0\n10.0\nq\n" (sut/main)))]
      (is (str/includes? out "DONE"))
      (is (str/includes? out "Accuracy:"))
      (is (str/includes? out "GoodBye!")))))
