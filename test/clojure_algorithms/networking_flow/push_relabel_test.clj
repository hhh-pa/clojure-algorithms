(ns clojure-algorithms.networking-flow.push-relabel-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.networking-flow.push-relabel :as pr]))

(def classic-caps
  {[0 1] 16 [0 2] 13 [1 2] 10 [1 3] 12
   [2 1] 4 [2 4] 14 [3 2] 9 [3 5] 20
   [4 3] 7 [4 5] 4})

(def small-caps
  {[0 1] 3 [0 2] 2 [1 2] 5 [1 3] 2 [2 3] 3})

(defn build-graph
  "Builds a flow network with `n` vertices from a {[u v] cap} map."
  [n caps]
  (reduce (fn [g [[u v] cap]] (pr/add-edge g u v cap))
          (pr/make-graph n)
          caps))

(deftest make-graph-test
  (testing "creates an empty graph"
    (let [g (pr/make-graph 3)]
      (is (= 3 (:size g)))
      (is (= [] (:edges g)))))
  (testing "rejects non-positive sizes"
    (is (thrown? IllegalArgumentException (pr/make-graph 0)))
    (is (thrown? IllegalArgumentException (pr/make-graph -1)))))

(deftest add-edge-test
  (testing "rejects negative capacity"
    (is (thrown? IllegalArgumentException
                 (pr/add-edge (pr/make-graph 2) 0 1 -1))))
  (testing "rejects out-of-range vertices"
    (is (thrown? IllegalArgumentException
                 (pr/add-edge (pr/make-graph 2) 2 0 1)))))

(deftest max-flow-test
  (testing "classic 6-vertex example"
    (is (= 23 (pr/max-flow (build-graph 6 classic-caps) 0 5))))
  (testing "smaller 4-vertex example"
    (is (= 5 (pr/max-flow (build-graph 4 small-caps) 0 3))))
  (testing "parallel edges add up"
    (let [g (-> (pr/make-graph 2)
                (pr/add-edge 0 1 3)
                (pr/add-edge 0 1 5))]
      (is (= 8 (pr/max-flow g 0 1)))))
  (testing "disconnected sink gives zero flow"
    (is (= 0 (pr/max-flow (pr/make-graph 3) 0 2))))
  (testing "input graph is not mutated"
    (let [g (build-graph 6 classic-caps)
          snapshot g]
      (is (= 23 (pr/max-flow g 0 5)))
      (is (= snapshot g))))
  (testing "invalid source/sink throw"
    (let [g (build-graph 4 small-caps)]
      (is (thrown? IllegalArgumentException (pr/max-flow g 0 0)))
      (is (thrown? IllegalArgumentException (pr/max-flow g 0 9))))))
