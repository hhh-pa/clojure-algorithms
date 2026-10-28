(ns clojure-algorithms.maths.aliquot-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.aliquot-sum :as sut]))

(deftest test-fn-aliquot-sum-examples
  (is (approx 9 (sut/aliquot-sum 15)))
  (is (approx 6 (sut/aliquot-sum 6)))
  (is (thrown? Exception (sut/aliquot-sum -1)))
  (is (thrown? Exception (sut/aliquot-sum 0)))
  (is (thrown? Exception (sut/aliquot-sum 1.6)))
  (is (approx 16 (sut/aliquot-sum 12)))
  (is (approx 0 (sut/aliquot-sum 1)))
  (is (approx 1 (sut/aliquot-sum 19))))

(deftest test-fn-aliquot-sum-extra
  (is (approx 28 (sut/aliquot-sum 28)))
  (is (approx 1 (sut/aliquot-sum 2)))
  (is (= :ValueError (try (sut/aliquot-sum -1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/aliquot-sum 1.6) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
