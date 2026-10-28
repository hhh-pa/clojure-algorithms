(ns clojure-algorithms.maths.average-mean-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.average-mean :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-mean-examples
  (is (approx 12.0 (sut/mean [3 6 9 12 15 18 21])))
  (is (approx 20.0 (sut/mean [5 10 15 20 25 30 35])))
  (is (approx 4.5 (sut/mean [1 2 3 4 5 6 7 8])))
  (is (thrown? Exception (sut/mean [])))
  (is (= :ValueError (err-type #(sut/mean [])))))

(deftest test-mean-extra
  (is (approx 5.0 (sut/mean [5])))
  (is (approx 2.5 (sut/mean [1.5 2.0 4.0])))
  (is (approx 0.0 (sut/mean [-1 0 1]))))
