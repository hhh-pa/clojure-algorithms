(ns clojure-algorithms.maths.sumset-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sumset :as sut]))

(deftest test-fn-sumset-examples
  (is (approx #{5 6 7 8 9} (sut/sumset (set [1 2 3]) (set [4 5 6]))))
  (is (approx #{5 6 7 8 9 10} (sut/sumset (set [1 2 3]) (set [4 5 6 7]))))
  (is (approx #{} (sut/sumset (set []) (set [1 2 3]))))
  ;; NOTE: drops.json quarantined (thrown? Exception (sut/sumset #{1 2 3 4} 3));
  ;; that form can never pass since assert throws AssertionError (an Error, not
  ;; an Exception). The covered equivalent is the AssertionError check below.
  (is (thrown? AssertionError (sut/sumset (set [1 2 3 4]) 3))))

(deftest test-fn-sumset-extra
  (is (thrown? AssertionError (sut/sumset (set [1 2 3 4]) 3)))
  (is (thrown? AssertionError (sut/sumset 3 (set [1 2 3])))))
