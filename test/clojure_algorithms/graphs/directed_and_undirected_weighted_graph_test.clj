(ns clojure-algorithms.graphs.directed-and-undirected-weighted-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.directed-and-undirected-weighted-graph
             :as g]))

(def demo
  (reduce (fn [gr [u v]] (g/add-pair gr u v))
          (g/make-graph)
          [[0 1] [0 2] [1 3] [1 4] [1 5] [2 5] [5 6]]))

(deftest test-directed-graph
  (let [gr (-> (g/make-graph)
               (g/add-pair -1 2) (g/add-pair 1 3 5)
               (g/add-pair 1 3 5) (g/add-pair 1 3 6))]
    (is (= [-1 2 1 3] (g/all-nodes gr)))
    (is (= [[5 3] [6 3]] (get-in gr [:adj 1]))))
  (is (= [] (g/all-nodes (g/make-graph))))
  (let [gr (g/add-pair (g/make-graph) 0 5 2)]
    (is (= [[2 5]] (get-in gr [:adj 0])))
    (is (= [] (get-in (g/remove-pair gr 0 5) [:adj 0])))
    (is (= [[2 5]] (get-in (g/remove-pair gr 5 0) [:adj 0]))))
  (is (= [] (g/dfs (g/make-graph))))
  (is (= [0 2 5 6] (g/dfs demo 0 6)))
  (is (= [1 5 6] (g/dfs demo 1 6)))
  (is (= [0 2 5 6 1 4 3] (g/dfs demo)))
  (is (= [1 5 6 4 3] (g/dfs demo 1 0)))
  (is (= [] (g/bfs (g/make-graph))))
  (is (= [0 1 2 3 4 5 6] (g/bfs demo 0)))
  (is (= [1 3 4 5 6] (g/bfs demo 1)))
  (is (= 2 (g/in-degree demo 5)))
  (is (= 3 (g/out-degree demo 1)))
  (let [order (g/topological-sort demo)
        pos (into {} (map-indexed (fn [i v] [v i]) order))]
    (is (= (set (range 7)) (set order)))
    (doseq [[u vs] (:adj demo) [_ v] vs]
      (is (< (get pos u) (get pos v)))))
  (is (false? (g/has-cycle? demo)))
  (is (true? (g/has-cycle? (g/add-pair demo 6 0))))
  (is (= #{0 1 2 5 6} (set (g/cycle-nodes (g/add-pair demo 6 0)))))
  (is (number? (g/dfs-time demo)))
  (is (number? (g/bfs-time demo))))

(deftest test-undirected-graph
  (let [gr (-> (g/make-graph false) (g/add-pair 1 2 3))]
    (is (= [[3 2]] (get-in gr [:adj 1])))
    (is (= [[3 1]] (get-in gr [:adj 2])))
    (is (= 1 (g/degree gr 1)))
    (is (false? (g/has-cycle? gr)))
    (let [tri (-> gr (g/add-pair 2 3) (g/add-pair 3 1))]
      (is (true? (g/has-cycle? tri)))
      (is (= #{1 2 3} (set (g/cycle-nodes tri))))))
  (let [gr (g/fill-graph-randomly (g/make-graph) 5)]
    (is (<= 5 (count (g/all-nodes gr))))))
