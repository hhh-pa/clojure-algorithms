(ns clojure-algorithms.maths.allocation-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.allocation-number :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-allocation-num-examples
  (is (approx ["1-4161" "4162-8322" "8323-12483" "12484-16647"]
              (sut/allocation-num 16647 4)))
  (is (approx ["1-10000" "10001-20000" "20001-30000" "30001-40000" "40001-50000"]
              (sut/allocation-num 50000 5)))
  (is (thrown? Exception (sut/allocation-num 888 999)))
  (is (thrown? Exception (sut/allocation-num 888 -4)))
  (is (= :ValueError (err-type #(sut/allocation-num 888 999))))
  (is (= :ValueError (err-type #(sut/allocation-num 888 -4))))
  (is (= :ValueError (err-type #(sut/allocation-num 100 0)))))

(deftest test-allocation-num-extra
  (is (= ["1-10"] (sut/allocation-num 10 1)))
  (is (= ["1-1" "2-2" "3-3"] (sut/allocation-num 3 3)))
  (is (= 4 (count (sut/allocation-num 16647 4)))))
