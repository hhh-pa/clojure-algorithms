(ns clojure-algorithms.divide-and-conquer.convex-hull-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.convex-hull :as ch]))

(def sample-10
  [[0 3] [2 2] [1 1] [2 1] [3 0] [0 0] [3 3] [2 -1] [2 -4] [1 -3]])

(def sample-10-hull
  [[0.0 0.0] [0.0 3.0] [1.0 -3.0] [2.0 -4.0] [3.0 0.0] [3.0 3.0]])

(deftest det-test
  (testing "signed distance"
    (is (= 0.0 (ch/det [1 1] [1 2] [1 5])))
    (is (= 100.0 (ch/det [0 0] [10 0] [0 10])))
    (is (= -100.0 (ch/det [0 0] [10 0] [0 -10])))))

(deftest brute-force-test
  (testing "convex-hull-bf"
    (is (= [[0.0 0.0] [1.0 0.0] [10.0 1.0]]
           (ch/convex-hull-bf [[0 0] [1 0] [10 1]])))
    (is (= [[0.0 0.0] [10.0 0.0]]
           (ch/convex-hull-bf [[0 0] [1 0] [10 0]])))
    (is (= sample-10-hull (ch/convex-hull-bf sample-10)))))

(deftest recursive-test
  (testing "convex-hull-recursive"
    (is (= [[0.0 0.0] [1.0 0.0] [10.0 1.0]]
           (ch/convex-hull-recursive [[0 0] [1 0] [10 1]])))
    (is (= [[0.0 0.0] [10.0 0.0]]
           (ch/convex-hull-recursive [[0 0] [1 0] [10 0]])))
    (is (= [[-1.0 -1.0] [-1.0 1.0] [1.0 -1.0] [1.0 1.0]]
           (ch/convex-hull-recursive [[-1 1] [-1 -1] [0 0] [0.5 0.5]
                                      [1 -1] [1 1] [-0.75 1]])))
    (is (= sample-10-hull (ch/convex-hull-recursive sample-10)))))

(deftest melkman-test
  (testing "convex-hull-melkman"
    (is (= [[0.0 0.0] [1.0 0.0] [10.0 1.0]]
           (ch/convex-hull-melkman [[0 0] [1 0] [10 1]])))
    (is (= [[0.0 0.0] [10.0 0.0]]
           (ch/convex-hull-melkman [[0 0] [1 0] [10 0]])))
    (is (= sample-10-hull (ch/convex-hull-melkman sample-10)))))

(deftest validation-test
  (testing "invalid input"
    (is (thrown? IllegalArgumentException (ch/convex-hull-bf [])))
    (is (thrown? IllegalArgumentException (ch/convex-hull-bf nil)))
    (is (thrown? IllegalArgumentException (ch/convex-hull-bf 1)))))
