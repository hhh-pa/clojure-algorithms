(ns clojure-algorithms.networking-flow.ford-fulkerson-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.networking-flow.ford-fulkerson :as ff]))

(def classic
  [[0 16 13 0 0 0]
   [0 0 10 12 0 0]
   [0 4 0 0 14 0]
   [0 0 9 0 0 20]
   [0 0 0 7 0 4]
   [0 0 0 0 0 0]])

(deftest breadth-first-search-test
  (testing "finds a path in the classic graph"
    (let [{:keys [found? parents]} (ff/breadth-first-search classic 0 5)]
      (is (true? found?))
      (is (= -1 (parents 0)))
      (is (= 0 (parents 1)))))
  (testing "reports unreachable sink"
    (let [{:keys [found?]} (ff/breadth-first-search [[0 0] [0 0]] 0 1)]
      (is (false? found?))))
  (testing "direct edge is found"
    (let [{:keys [found? parents]} (ff/breadth-first-search [[0 7] [0 0]] 0 1)]
      (is (true? found?))
      (is (= [-1 0] parents)))))

(deftest ford-fulkerson-test
  (testing "classic 6-vertex example"
    (is (= 23 (ff/ford-fulkerson classic 0 5))))
  (testing "smaller 4-vertex example"
    (is (= 5 (ff/ford-fulkerson [[0 3 2 0]
                                 [0 0 5 2]
                                 [0 0 0 3]
                                 [0 0 0 0]]
                                0 3))))
  (testing "single edge"
    (is (= 7 (ff/ford-fulkerson [[0 7] [0 0]] 0 1))))
  (testing "disconnected sink gives zero flow"
    (is (= 0 (ff/ford-fulkerson [[0 0] [0 0]] 0 1)))
    (is (= 0 (ff/ford-fulkerson [[0 5 0] [0 0 0] [0 0 0]] 0 2))))
  (testing "bottleneck limits flow"
    (is (= 1 (ff/ford-fulkerson [[0 10 0] [0 0 1] [0 0 0]] 0 2)))
    (is (= 11 (ff/ford-fulkerson [[0 10 10] [0 0 1] [0 0 0]] 0 2))))
  (testing "input graph is not modified"
    (let [g [[0 16 13 0 0 0]
             [0 0 10 12 0 0]
             [0 4 0 0 14 0]
             [0 0 9 0 0 20]
             [0 0 0 7 0 4]
             [0 0 0 0 0 0]]
          snapshot g]
      (ff/ford-fulkerson g 0 5)
      (is (= snapshot g))))
  (testing "invalid vertices throw"
    (is (thrown? IllegalArgumentException (ff/ford-fulkerson classic 0 0)))
    (is (thrown? IllegalArgumentException (ff/ford-fulkerson classic 0 6)))
    (is (thrown? IllegalArgumentException (ff/ford-fulkerson classic -1 5)))))
