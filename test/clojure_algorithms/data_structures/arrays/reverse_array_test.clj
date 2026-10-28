(ns clojure-algorithms.data-structures.arrays.reverse-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.reverse-array
             :refer [reverse-array]]))

(deftest test-reverse-array
  (is (= [5 4 3 2 1] (reverse-array [1 2 3 4 5])))
  (is (= ["d" "c" "b" "a"] (reverse-array ["a" "b" "c" "d"])))
  (is (= [30.8 20.2 10.5] (reverse-array [10.5 20.2 30.8])))
  (is (= ["cherry" "banana" "apple"]
         (reverse-array ["apple" "banana" "cherry"])))
  (is (= [1] (reverse-array [1])))
  (is (= [] (reverse-array [])))
  (is (= [4 3 2 1 0] (reverse-array (range 5))))
  (is (thrown? clojure.lang.ExceptionInfo (reverse-array 42))))
