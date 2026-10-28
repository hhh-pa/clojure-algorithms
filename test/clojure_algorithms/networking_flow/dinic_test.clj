(ns clojure-algorithms.networking-flow.dinic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.networking-flow.dinic :as d]))

(def classic-caps
  {[0 1] 16 [0 2] 13 [1 2] 10 [1 3] 12
   [2 1] 4 [2 4] 14 [3 2] 9 [3 5] 20
   [4 3] 7 [4 5] 4})

(def small-caps
  {[0 1] 3 [0 2] 2 [1 2] 5 [1 3] 2 [2 3] 3})

(defn build-graph
  "Builds a flow network with `n` vertices from a {[u v] cap} map."
  [n caps]
  (reduce (fn [g [[u v] cap]] (d/add-edge g u v cap))
          (d/make-graph n)
          caps))

(deftest make-graph-test
  (testing "creates an empty graph"
    (let [g (d/make-graph 3)]
      (is (= 3 (:size g)))
      (is (= [] (:edges g)))))
  (testing "rejects non-positive sizes"
    (is (thrown? IllegalArgumentException (d/make-graph 0)))
    (is (thrown? IllegalArgumentException (d/make-graph -2)))))

(deftest add-edge-test
  (testing "rejects negative capacity"
    (is (thrown? IllegalArgumentException
                 (d/add-edge (d/make-graph 2) 0 1 -1))))
  (testing "rejects out-of-range vertices"
    (is (thrown? IllegalArgumentException
                 (d/add-edge (d/make-graph 2) 0 2 5)))
    (is (thrown? IllegalArgumentException
                 (d/add-edge (d/make-graph 2) -1 1 5)))))

(deftest max-flow-test
  (testing "classic 6-vertex example"
    (is (= 23 (d/max-flow (build-graph 6 classic-caps) 0 5))))
  (testing "smaller 4-vertex example"
    (is (= 5 (d/max-flow (build-graph 4 small-caps) 0 3))))
  (testing "disconnected sink gives zero flow"
    (is (= 0 (d/max-flow (d/make-graph 3) 0 2))))
  (testing "parallel edges add up"
    (let [g (-> (d/make-graph 2)
                (d/add-edge 0 1 3)
                (d/add-edge 0 1 5))]
      (is (= 8 (d/max-flow g 0 1)))))
  (testing "input graph is not mutated"
    (let [g (build-graph 6 classic-caps)
          snapshot g]
      (is (= 23 (d/max-flow g 0 5)))
      (is (= snapshot g))
      (is (= 23 (d/max-flow g 0 5)))))
  (testing "invalid source/sink throw"
    (let [g (build-graph 4 small-caps)]
      (is (thrown? IllegalArgumentException (d/max-flow g 0 0)))
      (is (thrown? IllegalArgumentException (d/max-flow g 0 4)))
      (is (thrown? IllegalArgumentException (d/max-flow g -1 3))))))
