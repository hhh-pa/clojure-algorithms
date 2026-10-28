(ns clojure-algorithms.scheduling.weighted-interval-scheduling-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.weighted-interval-scheduling :as wis]))

(deftest weighted-interval-scheduling-test
  (testing "optimal compatible subset"
    (let [[w sel] (wis/weighted-interval-scheduling
                   [(wis/make-interval 0 3 3) (wis/make-interval 1 5 4)
                    (wis/make-interval 4 6 2) (wis/make-interval 6 8 5)])]
      (is (= 10.0 w))
      (is (= [[0 3 3] [4 6 2] [6 8 5]]
             (mapv (juxt :start :end :weight) sel)))))
  (testing "greedy earliest-finish fails here"
    (let [[w sel] (wis/weighted-interval-scheduling
                   [(wis/make-interval 1 4 10) (wis/make-interval 3 6 12)
                    (wis/make-interval 5 8 10)])]
      (is (= 20.0 w))
      (is (= [[1 4 10] [5 8 10]]
             (mapv (juxt :start :end :weight) sel)))))
  (testing "empty and single inputs"
    (is (= [0.0 []] (wis/weighted-interval-scheduling [])))
    (let [[w sel] (wis/weighted-interval-scheduling
                   [(wis/make-interval 2 5 7.5)])]
      (is (= 7.5 w))
      (is (= [[2 5 7.5]] (mapv (juxt :start :end :weight) sel)))))
  (testing "overlapping intervals keep the heaviest"
    (let [[w sel] (wis/weighted-interval-scheduling
                   [(wis/make-interval 1 10 5) (wis/make-interval 2 9 15)
                    (wis/make-interval 3 8 8)])]
      (is (= 15.0 w))
      (is (= [[2 9 15]] (mapv (juxt :start :end :weight) sel)))))
  (testing "disjoint intervals are all kept"
    (let [[w sel] (wis/weighted-interval-scheduling
                   [(wis/make-interval 1 2 5) (wis/make-interval 3 4 6)
                    (wis/make-interval 5 6 7)])]
      (is (= 18.0 w))
      (is (= 3 (count sel))))))

(deftest make-interval-test
  (testing "validation errors"
    (is (thrown? clojure.lang.ExceptionInfo (wis/make-interval 5 2 10)))
    (is (thrown? clojure.lang.ExceptionInfo (wis/make-interval 1 5 -3)))))
