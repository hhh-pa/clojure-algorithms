(ns clojure-algorithms.data-structures.stacks.kth-next-greater-element-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.kth-next-greater-element
             :refer [test-k test-array expected-answers
                     find-kth-next-greater-element]]))

(deftest test-kth-nge
  (is (= [4 5 nil nil nil] (find-kth-next-greater-element [1 2 3 4 5] 3)))
  (is (= [4.3 4.3 6.0 nil]
         (find-kth-next-greater-element [2.5 1.9 4.3 6.0] 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (find-kth-next-greater-element [1 2 3] 0)))
  (is (= (vec (repeat 1000 nil))
         (find-kth-next-greater-element (vec (range 1000)) 1000)))
  (is (= expected-answers (find-kth-next-greater-element test-array test-k))))
