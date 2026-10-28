(ns clojure-algorithms.machine-learning.word-frequency-functions-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.word-frequency-functions :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

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
