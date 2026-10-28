(ns clojure-algorithms.maths.sieve-of-atkin-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sieve-of-atkin :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-sieve-of-atkin-examples
  (is (approx [2 3 5 7 11 13 17 19 23 29] (sut/sieve-of-atkin 30)))
  (is (approx [2 3 5 7] (sut/sieve-of-atkin 10)))
  (is (approx [2] (sut/sieve-of-atkin 2)))
  (is (approx [] (sut/sieve-of-atkin 1)))
  (is (approx [] (sut/sieve-of-atkin 0)))
  (is (thrown? Exception (sut/sieve-of-atkin -5)))
  (is (= :ValueError (err-type #(sut/sieve-of-atkin -5)))))

(deftest test-sieve-of-atkin-extra
  (is (= 25 (count (sut/sieve-of-atkin 100))))
  (is (= [2 3 5] (sut/sieve-of-atkin 5)))
  (is (= [2 3] (sut/sieve-of-atkin 3))))
