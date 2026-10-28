(ns clojure-algorithms.maths.prime-factors-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.prime-factors :as sut]))

(deftest test-fn-prime-factors-examples
  (is (approx [] (sut/prime-factors 0)))
  (is (approx [2 2 5 5] (sut/prime-factors 100)))
  (is (approx [2 2 2 2 2 2 2 2 2 5] (sut/prime-factors 2560)))
  (is (approx [] (sut/prime-factors 0.01)))
  (is (approx [] (sut/prime-factors 0.02)))
  (is (approx [] (sut/prime-factors 0.0)))
  (is (thrown? Exception (sut/prime-factors "hello")))
  (is (thrown? Exception (sut/prime-factors [1 2 "hello"]))))

(deftest test-fn-unique-prime-factors-examples
  (is (approx [] (sut/unique-prime-factors 0)))
  (is (approx [2 5] (sut/unique-prime-factors 100)))
  (is (approx [2 5] (sut/unique-prime-factors 2560)))
  (is (approx [] (sut/unique-prime-factors 0.01)))
  (is (approx [] (sut/unique-prime-factors 0.02)))
  (is (approx [2 5] (sut/unique-prime-factors 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
  (is (approx [] (sut/unique-prime-factors 0.0)))
  (is (thrown? Exception (sut/unique-prime-factors "hello")))
  (is (thrown? Exception (sut/unique-prime-factors [1 2 "hello"]))))
