(ns clojure-algorithms.data-structures.linked-list.from-sequence-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.from-sequence
             :refer [make-linked-list node->str]]))

(deftest test-from-sequence
  (is (thrown? clojure.lang.ExceptionInfo (make-linked-list [])))
  (is (thrown? clojure.lang.ExceptionInfo (make-linked-list ())))
  (is (= "<1> ---> <END>" (node->str (make-linked-list [1]))))
  (is (= "<1> ---> <END>" (node->str (make-linked-list (list 1)))))
  (is (= "<1> ---> <3> ---> <5> ---> <32> ---> <44> ---> <12> ---> <43> ---> <END>"
         (node->str (make-linked-list [1 3 5 32 44 12 43]))))
  (is (= "<1> ---> <3> ---> <5> ---> <32> ---> <44> ---> <12> ---> <43> ---> <END>"
         (node->str (make-linked-list (list 1 3 5 32 44 12 43))))))
