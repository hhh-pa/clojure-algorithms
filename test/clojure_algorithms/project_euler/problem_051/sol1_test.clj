(ns clojure-algorithms.project-euler.problem-051.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-051.sol1 :refer [prime-sieve digit-replacements]]))

(deftest prime-sieve-test
  (is (= [2] (prime-sieve 3)))
  (is (= [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47] (prime-sieve 50))))

(deftest digit-replacements-test
  (is (= [[500 511 522 533 544 555 566 577 588 599]]
         (digit-replacements 544)))
  (is (= [[3002 3112 3222 3332 3442 3552 3662 3772 3882 3992]]
         (digit-replacements 3112))))
