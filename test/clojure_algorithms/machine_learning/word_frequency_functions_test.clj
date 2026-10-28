(ns clojure-algorithms.machine-learning.word-frequency-functions-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.word-frequency-functions :as sut]))

(deftest test-fn-term-frequency-examples
  (is (thrown? Exception (sut/term-frequency "to" "To be, or not to be"))))

(deftest test-fn-document-frequency-examples
  (is (thrown? Exception (sut/document-frequency "first" "This is the first document in the corpus.\nThIsis the second document in the corpus.\nTHIS is the third document in the corpus."))))

(deftest test-fn-inverse-document-frequency-examples
  (is (thrown? Exception (sut/inverse-document-frequency 3 0)))
  (is (approx 0.477 (sut/inverse-document-frequency 1 3)))
  (is (thrown? Exception (sut/inverse-document-frequency 0 3)))
  (is (approx 1.477 (sut/inverse-document-frequency 0 3 true))))

(deftest test-fn-tf-idf-examples
  (is (approx 0.954 (sut/tf-idf 2 0.477))))
