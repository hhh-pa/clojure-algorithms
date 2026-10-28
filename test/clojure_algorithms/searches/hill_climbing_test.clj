(ns clojure-algorithms.searches.hill-climbing-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.hill-climbing :as hc]))

(deftest score-test
  (testing "score evaluates f at (x, y)"
    (let [f (fn [x y] (+ x y))]
      (is (= 0 (hc/score (hc/make-search-problem 0 0 1 f))))
      (is (= 12 (hc/score (hc/make-search-problem 5 7 1 f)))))))

(deftest state-str-test
  (testing "string form"
    (is (= "x: 0 y: 0" (hc/state-str (hc/make-search-problem 0 0 1 nil))))
    (is (= "x: 2 y: 5" (hc/state-str (hc/make-search-problem 2 5 1 nil))))))

(deftest get-neighbors-test
  (testing "eight adjacent states"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          nbrs (hc/get-neighbors (hc/make-search-problem 0 0 1 f))]
      (is (= 8 (count nbrs)))
      (is (= #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]}
             (set (map (juxt :x :y) nbrs)))))))

(deftest hill-climbing-test
  (testing "minimize x^2 + y^2 from (3, 4)"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          result (hc/hill-climbing (hc/make-search-problem 3 4 1 f)
                                   {:find-max false})]
      (is (= 0 (:x result)))
      (is (= 0 (:y result)))
      (is (= 0 (hc/score result)))))
  (testing "bounded maximize reaches the corner"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          result (hc/hill-climbing (hc/make-search-problem 3 4 1 f)
                                   {:find-max true :max-x 5 :min-x -5
                                    :max-y 5 :min-y -5})]
      (is (= 5 (:x result)))
      (is (= 5 (:y result)))
      (is (= 50 (hc/score result)))))
  (testing "already optimal state stays put"
    (let [f (fn [x y] (+ (* x x) (* y y)))
          start (hc/make-search-problem 0 0 1 f)]
      (is (= start (hc/hill-climbing start {:find-max false}))))))
