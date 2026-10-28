(ns clojure-algorithms.maths.basic-maths-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.basic-maths :as sut]))

(deftest test-fn-prime-factors-examples
  (is (approx [2 2 5 5] (sut/prime-factors 100)))
  (is (thrown? Exception (sut/prime-factors 0)))
  (is (thrown? Exception (sut/prime-factors -10))))

(deftest test-fn-number-of-divisors-examples
  (is (approx 9 (sut/number-of-divisors 100)))
  (is (thrown? Exception (sut/number-of-divisors 0)))
  (is (thrown? Exception (sut/number-of-divisors -10))))

(deftest test-fn-sum-of-divisors-examples
  (is (approx 217 (sut/sum-of-divisors 100)))
  (is (thrown? Exception (sut/sum-of-divisors 0)))
  (is (thrown? Exception (sut/sum-of-divisors -10))))

(deftest test-fn-euler-phi-examples
  (is (approx 40 (sut/euler-phi 100)))
  (is (thrown? Exception (sut/euler-phi 0)))
  (is (thrown? Exception (sut/euler-phi -10))))

(deftest test-fn-basic-maths-extra
  (is (approx [13] (sut/prime-factors 13)))
  (is (approx [] (sut/prime-factors 1)))
  (is (approx [2 3] (sut/prime-factors 6)))
  (is (approx 1 (sut/number-of-divisors 1)))
  (is (approx 2 (sut/number-of-divisors 7)))
  (is (approx 1 (sut/sum-of-divisors 1)))
  (is (approx 1 (sut/euler-phi 1)))
  (is (approx 6 (sut/euler-phi 7)))
  (is (= :ValueError (try (sut/prime-factors 0) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/euler-phi -3) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
