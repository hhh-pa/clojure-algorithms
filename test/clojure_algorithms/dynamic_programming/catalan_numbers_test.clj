(ns clojure-algorithms.dynamic-programming.catalan-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.catalan-numbers :as cn]))

(deftest catalan-numbers-test
  (testing "Catalan sequences"
    (is (= [1 1 2 5 14 42] (cn/catalan-numbers 5)))
    (is (= [1 1 2] (cn/catalan-numbers 2)))
    (is (= [1] (cn/catalan-numbers 0)))
    (is (= [1 1] (cn/catalan-numbers 1)))
    (is (thrown? IllegalArgumentException (cn/catalan-numbers -1)))))
