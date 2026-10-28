(ns clojure-algorithms.dynamic-programming.floyd-warshall-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.floyd-warshall :as fw]))

(deftest floyd-warshall-test
  (testing "add-edge"
    (let [g (fw/add-edge (fw/make-graph 3) 0 1 5)]
      (is (= 5 (get-in (:dp g) [0 1])))))
  (testing "shortest paths"
    (let [g (-> (fw/make-graph 3)
                (fw/add-edge 0 1 1)
                (fw/add-edge 1 2 2)
                (fw/floyd-warshall))]
      (is (= 3 (fw/show-min g 0 2)))
      (is (= Double/POSITIVE_INFINITY (fw/show-min g 2 0)))))
  (testing "longer chain"
    (let [g (-> (fw/make-graph 3)
                (fw/add-edge 0 1 3)
                (fw/add-edge 1 2 4)
                (fw/floyd-warshall))]
      (is (= 7 (fw/show-min g 0 2)))
      (is (= Double/POSITIVE_INFINITY (fw/show-min g 1 0)))))
  (testing "five-node example"
    (let [g (-> (fw/make-graph 5)
                (fw/add-edge 0 2 9) (fw/add-edge 0 4 10)
                (fw/add-edge 1 3 5) (fw/add-edge 2 3 7)
                (fw/add-edge 3 0 10) (fw/add-edge 3 1 2)
                (fw/add-edge 3 2 1) (fw/add-edge 3 4 6)
                (fw/add-edge 4 1 3) (fw/add-edge 4 2 4)
                (fw/add-edge 4 3 9)
                (fw/floyd-warshall))]
      (is (= 11 (fw/show-min g 1 4)))
      (is (= 16 (fw/show-min g 0 3))))))
