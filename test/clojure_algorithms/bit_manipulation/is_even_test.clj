(ns clojure-algorithms.bit-manipulation.is-even-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.is-even :refer [is-even is-even-using-shift-operator]]))

(deftest is-even-test
  (testing "doctest examples"
    (doseq [f [is-even is-even-using-shift-operator]]
      (is (false? (f 1)))
      (is (true? (f 4)))
      (is (false? (f 9)))
      (is (false? (f 15)))
      (is (true? (f 40)))
      (is (true? (f 100)))
      (is (false? (f 101)))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (is-even true)))
    (is (thrown? clojure.lang.ExceptionInfo (is-even 3.14)))))
