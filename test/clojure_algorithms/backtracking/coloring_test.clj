(ns clojure-algorithms.backtracking.coloring-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.coloring :refer [valid-coloring? util-color color]]))

(def ^:private sample-graph
  [[0 1 0 0 0]
   [1 0 1 0 1]
   [0 1 0 1 0]
   [0 1 1 0 0]
   [0 1 0 0 0]])

(deftest valid-coloring-test
  (is (true? (valid-coloring? [0 1 0 1 0] [0 2 1 2 0] 1)))
  (is (false? (valid-coloring? [0 1 0 1 0] [0 2 1 2 0] 2)))
  (is (true? (valid-coloring? [1 0 1 0] [-1 -1 -1 -1] 0))))

(deftest util-color-test
  (is (= [0 1 0 2 0] (util-color sample-graph 3 [0 1 0 0 0] 3)))
  (is (nil? (util-color sample-graph 2 [0 1 0 0 0] 3))))

(deftest color-test
  (testing "doctest examples"
    (is (= [0 1 0 2 0] (color sample-graph 3)))
    (is (= [] (color sample-graph 2)))
    (is (= [0 1] (color [[0 1] [1 0]] 2)))
    (is (= [] (color [[0 1 1] [1 0 1] [1 1 0]] 2)))
    (is (= [0 1 2] (color [[0 1 1] [1 0 1] [1 1 0]] 3)))
    (is (= [] (color [] 2)))
    (is (= [0] (color [[0]] 1)))
    (is (= [] (color [[0 1] [1 0]] 1)))
    (is (= [0 1] (color [[0 1] [1 0]] 2))))
  (testing "result satisfies the coloring constraint"
    (let [assignment (color sample-graph 3)]
      (doseq [i (range (count sample-graph))
              j (range (count sample-graph))
              :when (= 1 (get-in sample-graph [i j]))]
        (is (not= (nth assignment i) (nth assignment j)))))))
