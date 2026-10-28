(ns clojure-algorithms.maths.gcd-of-n-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.gcd-of-n-numbers :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-get-factors-examples
  (is (= {3 2, 5 1} (sut/get-factors 45)))
  (is (= {2 3, 3 2, 5 1, 7 1} (sut/get-factors 2520)))
  (is (= {23 1} (sut/get-factors 23)))
  (is (thrown? Exception (sut/get-factors 0)))
  (is (thrown? Exception (sut/get-factors -1)))
  (is (thrown? Exception (sut/get-factors 1.5)))
  (is (= :TypeError (err-type #(sut/get-factors 0))))
  (is (= :TypeError (err-type #(sut/get-factors -1))))
  (is (= :TypeError (err-type #(sut/get-factors 1.5)))))

(deftest test-fn-get-greatest-common-divisor-examples
  (is (= 9 (sut/get-greatest-common-divisor 18 45)))
  (is (= 1 (sut/get-greatest-common-divisor 23 37)))
  (is (= 10 (sut/get-greatest-common-divisor 2520 8350)))
  (is (thrown? Exception (sut/get-greatest-common-divisor -10 20)))
  (is (thrown? Exception (sut/get-greatest-common-divisor 1.5 2)))
  (is (= 1 (sut/get-greatest-common-divisor 1 2 3 4 5 6 7 8 9 10)))
  (is (thrown? Exception (sut/get-greatest-common-divisor "1" 2 3 4 5 6 7 8 9 10)))
  (is (= :Exception (err-type #(sut/get-greatest-common-divisor -10 20))))
  (is (= :Exception (err-type #(sut/get-greatest-common-divisor 1.5 2))))
  (is (= :Exception (err-type #(sut/get-greatest-common-divisor "1" 2)))))

(deftest test-gcd-extra
  (is (= {1 1} (sut/get-factors 1)))
  (is (= {2 1} (sut/get-factors 2)))
  (is (= 6 (sut/get-greatest-common-divisor 12 18 24)))
  (is (= 7 (sut/get-greatest-common-divisor 7))))
