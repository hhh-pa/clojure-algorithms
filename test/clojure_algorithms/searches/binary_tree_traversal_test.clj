(ns clojure-algorithms.searches.binary-tree-traversal-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.binary-tree-traversal :as bt]))

(defn- sample-tree
  "Balanced tree of 1..7 matching the Python doctests."
  []
  (bt/make-node 1
                (bt/make-node 2 (bt/make-node 4) (bt/make-node 5))
                (bt/make-node 3 (bt/make-node 6) (bt/make-node 7))))

(deftest traversals-test
  (testing "recursive traversals"
    (let [root (sample-tree)]
      (is (= [1 2 4 5 3 6 7] (bt/pre-order root)))
      (is (= [4 2 5 1 6 3 7] (bt/in-order root)))
      (is (= [4 5 2 6 7 3 1] (bt/post-order root)))
      (is (= [1 2 3 4 5 6 7] (bt/level-order root)))
      (is (= [[1] [2 3] [4 5 6 7]] (bt/level-order-actual root)))))
  (testing "iterative traversals agree"
    (let [root (sample-tree)]
      (is (= [1 2 4 5 3 6 7] (bt/pre-order-iter root)))
      (is (= [4 2 5 1 6 3 7] (bt/in-order-iter root)))
      (is (= [4 5 2 6 7 3 1] (bt/post-order-iter root)))))
  (testing "nil input yields nil"
    (is (nil? (bt/pre-order nil)))
    (is (nil? (bt/level-order nil)))))

(deftest build-tree-test
  (testing "level-order construction"
    (let [root (bt/build-tree [1 2 3 4 5 6 7])]
      (is (= [1 2 4 5 3 6 7] (bt/pre-order root)))
      (is (= [1 2 3 4 5 6 7] (bt/level-order root))))
    (is (nil? (bt/build-tree [])))
    (is (= [9] (bt/pre-order (bt/build-tree [9]))))))

(deftest prompt-test
  (testing "padding widths"
    (is (= [" Python " " Python *" "* Python *" "* Python **" "** Python **"]
           (mapv #(bt/prompt "Python" %) (range 8 13)))))
  (testing "exact width and negative width"
    (is (= 50 (count (bt/prompt "Python"))))
    (is (= " Python " (bt/prompt "Python" -200))))
  (testing "empty string"
    (is (= (str "\n" (apply str (repeat 50 "*"))) (bt/prompt "")))))
