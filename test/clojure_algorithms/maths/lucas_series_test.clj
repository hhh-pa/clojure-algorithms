(ns clojure-algorithms.maths.lucas-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.lucas-series :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-recursive-lucas-number-examples
  (is (approx 1 (sut/recursive-lucas-number 1)))
  (is (approx 15127 (sut/recursive-lucas-number 20)))
  (is (approx 2 (sut/recursive-lucas-number 0)))
  (is (approx 167761 (sut/recursive-lucas-number 25)))
  (is (thrown? Exception (sut/recursive-lucas-number -1.5)))
  (is (= :TypeError (err-type #(sut/recursive-lucas-number -1.5))))
  (is (= :TypeError (err-type #(sut/recursive-lucas-number "3")))))

(deftest test-fn-dynamic-lucas-number-examples
  (is (approx 1 (sut/dynamic-lucas-number 1)))
  (is (approx 15127 (sut/dynamic-lucas-number 20)))
  (is (approx 2 (sut/dynamic-lucas-number 0)))
  (is (approx 167761 (sut/dynamic-lucas-number 25)))
  (is (thrown? Exception (sut/dynamic-lucas-number -1.5)))
  (is (= :TypeError (err-type #(sut/dynamic-lucas-number -1.5))))
  (is (= :TypeError (err-type #(sut/dynamic-lucas-number nil)))))

(deftest test-lucas-agreement
  (is (= (mapv sut/recursive-lucas-number (range 15))
         (mapv sut/dynamic-lucas-number (range 15))))
  (is (= [2 1 3 4 7 11 18] (mapv sut/dynamic-lucas-number (range 7)))))
