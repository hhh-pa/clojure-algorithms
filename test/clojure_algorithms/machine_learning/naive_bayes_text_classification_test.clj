(ns clojure-algorithms.machine-learning.naive-bayes-text-classification-test
  (:require [clojure-algorithms.machine-learning.naive-bayes-text-classification
             :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(defn- fitted-model
  ([] (fitted-model 1.0))
  ([alpha]
   (let [[texts labels] (sut/build-toy-dataset)]
     (sut/NaiveBayesTextClassifier-fit
      (sut/make-NaiveBayesTextClassifier alpha) texts labels))))

(deftest test-fn-constructor-errors
  (is (thrown? Exception (sut/make-NaiveBayesTextClassifier 0)))
  (is (= :ValueError (:type (try (sut/make-NaiveBayesTextClassifier -1.0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
  (testing "defaults to alpha 1.0, unfitted"
    (let [model (sut/make-NaiveBayesTextClassifier)]
      (is (= 1.0 (:alpha model)))
      (is (false? (:is-fitted- model))))))

(deftest test-fn-tokenize-examples
  (is (= ["hello" "nlp" "world"]
         (sut/NaiveBayesTextClassifier-tokenize "Hello, NLP world!")))
  (is (= [] (sut/NaiveBayesTextClassifier-tokenize "")))
  (is (= ["don't" "stop"] (sut/NaiveBayesTextClassifier-tokenize "Don't stop"))))

(deftest test-fn-fit-examples
  (let [model (sut/NaiveBayesTextClassifier-fit
               (sut/make-NaiveBayesTextClassifier)
               ["cheap meds" "project meeting"] ["spam" "ham"])]
    (is (= ["ham" "spam"] (:classes- model)))
    (is (true? (:is-fitted- model))))
  (is (thrown? Exception
               (sut/NaiveBayesTextClassifier-fit
                (sut/make-NaiveBayesTextClassifier) ["only one text"]
                ["ham" "spam"])))
  (is (thrown? Exception (sut/NaiveBayesTextClassifier-fit
                          (sut/make-NaiveBayesTextClassifier) [] [])))
  (is (= :TypeError (:type (try (sut/NaiveBayesTextClassifier-fit
                                 (sut/make-NaiveBayesTextClassifier) [42] ["ham"])
                                (catch clojure.lang.ExceptionInfo e
                                  (ex-data e))))))
  (testing "fit is pure: the input model is unchanged"
    (let [model (sut/make-NaiveBayesTextClassifier)]
      (sut/NaiveBayesTextClassifier-fit model ["a b"] ["ham"])
      (is (false? (:is-fitted- model))))))

(deftest test-fn-predict-proba-examples
  (let [model (fitted-model)
        probs (sut/NaiveBayesTextClassifier-predict-proba
               model "cheap meds available now")]
    (is (approx 1.0 (reduce + (vals probs))))
    (is (> (get probs "spam") (get probs "ham"))))
  (testing "empty text falls back to the class priors"
    (let [probs (sut/NaiveBayesTextClassifier-predict-proba (fitted-model) "")]
      (is (approx 0.5 (get probs "spam")))
      (is (approx 0.5 (get probs "ham")))))
  (is (thrown? Exception (sut/NaiveBayesTextClassifier-predict-proba
                          (sut/make-NaiveBayesTextClassifier) "hello")))
  (is (= :TypeError (:type (try (sut/NaiveBayesTextClassifier-predict-proba
                                 (fitted-model) 42)
                                (catch clojure.lang.ExceptionInfo e
                                  (ex-data e)))))))

(deftest test-fn-predict-examples
  (let [model (fitted-model)]
    (is (= "spam" (sut/NaiveBayesTextClassifier-predict model "free cheap meds")))
    (is (= "ham" (sut/NaiveBayesTextClassifier-predict
                  model "project meeting schedule")))))

(deftest test-fn-build-toy-dataset-examples
  (let [[texts labels] (sut/build-toy-dataset)]
    (is (= 6 (count texts) (count labels)))
    (is (= ["ham" "spam"] (sort (set labels))))))
