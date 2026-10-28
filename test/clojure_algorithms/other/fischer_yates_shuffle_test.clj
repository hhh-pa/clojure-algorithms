(ns clojure-algorithms.other.fischer-yates-shuffle-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.fischer-yates-shuffle :as f]))

(deftest fisher-yates-shuffle-test
  (testing "result is a permutation of the input"
    (let [in [0 1 2 3 4 5 6 7]]
      (is (= (sort in) (sort (f/fisher-yates-shuffle in))))))
  (testing "deterministic with injected rand-fn"
    (is (= (f/fisher-yates-shuffle [1 2 3 4] (fn [_] 0))
           (f/fisher-yates-shuffle [1 2 3 4] (fn [_] 0)))))
  (testing "identity rand keeps multiset"
    (is (= [1 2 3] (sort (f/fisher-yates-shuffle [1 2 3] (fn [n] (dec n)))))))
  (testing "empty and singletons"
    (is (= [] (f/fisher-yates-shuffle [])))
    (is (= [1] (f/fisher-yates-shuffle [1])))))
