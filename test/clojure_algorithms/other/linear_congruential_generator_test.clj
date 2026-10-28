(ns clojure-algorithms.other.linear-congruential-generator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.linear-congruential-generator :as l]))

(deftest lcg-test
  (testing "known sequence for small parameters"
    (let [lcg (l/make-lcg 2 3 10 1)
          [s1 v1] (l/next-number lcg)
          [s2 v2] (l/next-number s1)
          [_ v3] (l/next-number s2)]
      (is (= [5 3 9] [v1 v2 v3]))))
  (testing "values stay within [0, modulo)"
    (let [lcg (l/make-lcg 1664525 1013904223 1024 42)]
      (is (every? #(and (>= % 0) (< % 1024)) (take 100 (l/lcg-seq lcg))))))
  (testing "same seed reproduces the sequence"
    (is (= (take 10 (l/lcg-seq (l/make-lcg 1664525 1013904223 1024 7)))
           (take 10 (l/lcg-seq (l/make-lcg 1664525 1013904223 1024 7)))))))
