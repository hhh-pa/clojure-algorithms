(ns clojure-algorithms.maths.two-pointer-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.two-pointer :as sut]))

(deftest test-fn-two-pointer-examples
  (is (approx [0 1] (sut/two-pointer [2 7 11 15] 9)))
  (is (approx [0 3] (sut/two-pointer [2 7 11 15] 17)))
  (is (approx [1 2] (sut/two-pointer [2 7 11 15] 18)))
  (is (approx [2 3] (sut/two-pointer [2 7 11 15] 26)))
  (is (approx [1 2] (sut/two-pointer [1 3 3] 6)))
  (is (approx [] (sut/two-pointer [2 7 11 15] 8)))
  (is (approx [] (sut/two-pointer (mapv #(* 3 %) (range 10)) 19)))
  (is (approx [] (sut/two-pointer [1 2 3] 6))))
