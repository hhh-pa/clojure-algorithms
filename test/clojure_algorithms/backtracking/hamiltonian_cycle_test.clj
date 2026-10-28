(ns clojure-algorithms.backtracking.hamiltonian-cycle-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.hamiltonian-cycle
             :refer [valid-connection? util-hamilton-cycle hamilton-cycle]]))

(def ^:private graph
  [[0 1 0 1 0]
   [1 0 1 1 1]
   [0 1 0 0 1]
   [1 1 0 0 1]
   [0 1 1 1 0]])

(deftest valid-connection-test
  (is (true? (valid-connection? graph 1 1 [0 -1 -1 -1 -1 0])))
  (is (false? (valid-connection? graph 1 4 [0 1 2 4 -1 0]))))

(deftest util-hamilton-cycle-test
  (is (= [0 1 2 4 3 0]
         (util-hamilton-cycle graph [0 -1 -1 -1 -1 0] 1)))
  (is (= [0 1 2 4 3 0]
         (util-hamilton-cycle graph [0 1 2 -1 -1 0] 3))))

(deftest hamilton-cycle-test
  (testing "doctest examples"
    (is (= [0 1 2 4 3 0] (hamilton-cycle graph)))
    (is (= [3 0 1 2 4 3] (hamilton-cycle graph 3)))
    (let [no-cycle [[0 1 0 1 0]
                    [1 0 1 1 1]
                    [0 1 0 0 1]
                    [1 1 0 0 0]
                    [0 1 1 0 0]]]
      (is (= [] (hamilton-cycle no-cycle 4)))))
  (testing "result is a valid cycle through every vertex"
    (let [cycle (hamilton-cycle graph)]
      (is (= (first cycle) (peek cycle)))
      (is (= (set (range 5)) (set (pop cycle))))
      (doseq [[a b] (partition 2 1 cycle)]
        (is (= 1 (get-in graph [a b])))))))
