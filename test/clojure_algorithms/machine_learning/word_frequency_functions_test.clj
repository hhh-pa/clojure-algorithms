(ns clojure-algorithms.machine-learning.word-frequency-functions-test
  (:require [clojure-algorithms.machine-learning.word-frequency-functions :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-term-frequency-examples
  (is (= 2 (sut/term-frequency "to" "To be, or not to be")))
  (testing "punctuation is stripped, matching is case-insensitive"
    (is (= 3 (sut/term-frequency "hello" "Hello, hello! HELLO.")))
    (is (= 0 (sut/term-frequency "missing" "To be, or not to be")))))

(deftest test-fn-document-frequency-examples
  (is (= [1 3] (sut/document-frequency
                "first"
                (str "This is the first document in the corpus.\n"
                     "ThIs is the second document in the corpus.\n"
                     "THIS is the third document in the corpus."))))
  (testing "substring match across documents"
    (is (= [3 3] (sut/document-frequency "corpus" "a corpus\nb corpus\nc corpus")))
    (is (= [0 2] (sut/document-frequency "zzz" "aaa\nbbb")))))

(deftest test-fn-inverse-document-frequency-examples
  (is (thrown? Exception (sut/inverse-document-frequency 3 0)))
  (is (approx 0.477 (sut/inverse-document-frequency 1 3)))
  (is (thrown? Exception (sut/inverse-document-frequency 0 3)))
  (is (approx 1.477 (sut/inverse-document-frequency 0 3 true)))
  (is (= :ValueError (:type (try (sut/inverse-document-frequency 3 0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
  (is (= :ZeroDivisionError (:type (try (sut/inverse-document-frequency 0 3)
                                        (catch clojure.lang.ExceptionInfo e
                                          (ex-data e)))))))

(deftest test-fn-tf-idf-examples
  (is (approx 0.954 (sut/tf-idf 2 0.477)))
  (is (approx 0.0 (sut/tf-idf 0 1.5))))
