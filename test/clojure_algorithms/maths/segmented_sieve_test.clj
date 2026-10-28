(ns clojure-algorithms.maths.segmented-sieve-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.segmented-sieve :as sut]))

(deftest test-fn-sieve-examples
  (is (approx [2 3 5 7] (sut/sieve 8)))
  (is (approx [2 3 5 7 11 13 17 19 23] (sut/sieve 27)))
  (is (thrown? Exception (sut/sieve 0)))
  (is (thrown? Exception (sut/sieve -1)))
  (is (thrown? Exception (sut/sieve 22.2))))

(deftest test-fn-sieve-extra
  (is (approx [] (sut/sieve 1)))
  (is (approx [2] (sut/sieve 2)))
  (is (approx [2 3 5 7 11 13 17 19 23 29] (sut/sieve 30)))
  (is (= :ValueError (try (sut/sieve 0)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :ValueError (try (sut/sieve 22.2)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))
