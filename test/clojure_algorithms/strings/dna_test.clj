(ns clojure-algorithms.strings.dna-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.dna :refer [dna]]))

(deftest test-dna
  (testing "doctest examples"
    (is (= "CGAT" (dna "GCTA")))
    (is (= "TACG" (dna "ATGC")))
    (is (= "GACT" (dna "CTGA"))))
  (testing "invalid strand"
    (is (thrown? IllegalArgumentException (dna "GFGG")))))
