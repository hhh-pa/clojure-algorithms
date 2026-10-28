(ns clojure-algorithms.dynamic-programming.factorial-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.factorial :as f]))

(deftest factorial-test
  (testing "factorials"
    (is (= 5040 (f/factorial 7)))
    (is (= [1 1 2 6 24 120 720 5040 40320 362880]
           (mapv f/factorial (range 10))))
    (is (thrown? IllegalArgumentException (f/factorial -1)))))
