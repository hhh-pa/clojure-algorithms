(ns clojure-algorithms.maths.interquartile-range-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.interquartile-range :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-find-median-examples
  (is (approx 2 (sut/find-median [1 2 2 3 4])))
  (is (approx 2.5 (sut/find-median [1 2 2 3 4 4])))
  (is (approx 1.5 (sut/find-median [-1 2 0 3 4 -4])))
  (is (approx 2.65 (sut/find-median [1.1 2.2 2 3.3 4.4 4]))))

(deftest test-fn-interquartile-range-examples
  (is (approx 2.0 (sut/interquartile-range [4 1 2 3 2])))
  (is (approx 17.0 (sut/interquartile-range [-2 -7 -10 9 8 4 -67 45])))
  (is (approx 17.2 (sut/interquartile-range [-2.1 -7.1 -10.1 9.1 8.1 4.1 -67.1 45.1])))
  (is (approx 0.0 (sut/interquartile-range [0 0 0 0 0])))
  (is (thrown? Exception (sut/interquartile-range [])))
  (is (= :ValueError (err-type #(sut/interquartile-range [])))))

(deftest test-interquartile-range-extra
  (is (approx 2 (sut/interquartile-range [1 2 3])))
  (is (approx 0.0 (sut/interquartile-range [7 7]))))
