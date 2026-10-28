(ns clojure-algorithms.project-euler.problem-102.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-102.sol1 :refer [contains-origin?
                                                                       solution
                                                                       vector-product]]))

(deftest vector-product-test
  (is (= 10 (vector-product [1 2] [-5 0])))
  (is (= 24 (vector-product [3 1] [6 10]))))

(deftest contains-origin?-test
  (is (true? (contains-origin? -340 495 -153 -910 835 -947)))
  (is (false? (contains-origin? -175 41 -421 -714 574 -645))))

(deftest solution-test
  (is (= 1 (solution "-340,495,-153,-910,835,-947\n-175,41,-421,-714,574,-645"))))
