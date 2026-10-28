(ns clojure-algorithms.maths.hardy-ramanujanalgo-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.hardy-ramanujanalgo :as sut]))

(deftest test-fn-exact-prime-factor-count-examples
  (is (approx 3 (sut/exact-prime-factor-count 51242183))))

(deftest test-fn-exact-prime-factor-count-extra
  (is (approx 0 (sut/exact-prime-factor-count 1)))
  (is (approx 1 (sut/exact-prime-factor-count 2)))
  (is (approx 1 (sut/exact-prime-factor-count 7)))
  (is (approx 2 (sut/exact-prime-factor-count 12)))
  (is (approx 2 (sut/exact-prime-factor-count 100)))
  (is (approx 3 (sut/exact-prime-factor-count 30)))
  (is (thrown? Exception (sut/exact-prime-factor-count 0)))
  (is (thrown? Exception (sut/exact-prime-factor-count -5))))
