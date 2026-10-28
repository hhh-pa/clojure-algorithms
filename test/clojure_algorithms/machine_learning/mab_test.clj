(ns clojure-algorithms.machine-learning.mab-test
  (:require [clojure-algorithms.machine-learning.mab :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-bandit
  (let [bandit (sut/make-Bandit [0.1 0.5 0.9])]
    (is (= 3 (:num-arms bandit)))
    (is (integer? (sut/Bandit-pull bandit 0))))
  (testing "degenerate probabilities pull deterministically"
    (is (= [1 1 1] (let [b (sut/make-Bandit [1.0] 1)]
                     [(sut/Bandit-pull b 0) (sut/Bandit-pull b 0)
                      (sut/Bandit-pull b 0)])))
    (is (= [0 0 0] (let [b (sut/make-Bandit [0.0] 1)]
                     [(sut/Bandit-pull b 0) (sut/Bandit-pull b 0)
                      (sut/Bandit-pull b 0)])))))

(deftest test-fn-epsilon-greedy
  (testing "selection stays in range"
    (let [s (sut/make-EpsilonGreedy 0.1 3 42)]
      (is (<= 0 (sut/EpsilonGreedy-select-arm s) 2))))
  (testing "update tracks counts and running mean"
    (let [s (sut/EpsilonGreedy-update (sut/make-EpsilonGreedy 0.1 3) 0 1)]
      (is (approx [1.0 0.0 0.0] (:counts s)))
      (is (approx [1.0 0.0 0.0] (:values s)))
      (let [s2 (sut/EpsilonGreedy-update s 0 0)]
        (is (approx [2.0 0.0 0.0] (:counts s2)))
        (is (approx [0.5 0.0 0.0] (:values s2))))))
  (testing "epsilon 0 exploits the best arm"
    (let [s (-> (sut/make-EpsilonGreedy 0.0 3 5)
                (sut/EpsilonGreedy-update 2 1))]
      (is (= 2 (sut/EpsilonGreedy-select-arm s))))))

(deftest test-fn-ucb
  (testing "untried arms are pulled first, in order"
    (let [s (sut/make-UCB 3)]
      (is (= 0 (sut/UCB-select-arm s)))
      (let [s (sut/UCB-update s 0 1)]
        (is (= 1 (sut/UCB-select-arm s)))
        (is (= 1 (:total-counts s)))
        (is (approx [1.0 0.0 0.0] (:counts s))))))
  (testing "after the first round the optimistic arm wins"
    (let [s (-> (sut/make-UCB 2)
                (sut/UCB-update 0 1)
                (sut/UCB-update 1 0))]
      (is (= 0 (sut/UCB-select-arm s))))))

(deftest test-fn-thompson-sampling
  (testing "selection stays in range"
    (let [s (sut/make-ThompsonSampling 3 42)]
      (is (<= 0 (sut/ThompsonSampling-select-arm s) 2))))
  (testing "successes and failures are counted"
    (let [s (-> (sut/make-ThompsonSampling 3)
                (sut/ThompsonSampling-update 0 1)
                (sut/ThompsonSampling-update 0 0))]
      (is (approx [1.0 0.0 0.0] (:successes s)))
      (is (approx [1.0 0.0 0.0] (:failures s)))))
  (testing "a dominant arm is heavily favoured"
    (let [s (reduce (fn [st _] (-> st
                                   (sut/ThompsonSampling-update 0 1)
                                   (sut/ThompsonSampling-update 1 0)))
                    (sut/make-ThompsonSampling 2 3)
                    (range 20))
          picks (repeatedly 50 #(sut/ThompsonSampling-select-arm s))]
      (is (> (count (filter zero? picks)) 40)))))

(deftest test-fn-random-strategy
  (let [s (sut/make-RandomStrategy 3 9)]
    (is (<= 0 (sut/RandomStrategy-select-arm s) 2))
    (is (= s (sut/RandomStrategy-update s 0 1)))))

(deftest test-fn-greedy-strategy
  (let [s (sut/make-GreedyStrategy 3)]
    (is (= 0 (sut/GreedyStrategy-select-arm s))))
  (testing "settles on the rewarded arm"
    (let [s (reduce (fn [st _] (sut/GreedyStrategy-update st 2 1))
                    (sut/make-GreedyStrategy 3) (range 10))]
      (is (= 2 (sut/GreedyStrategy-select-arm
                (-> s
                    (sut/GreedyStrategy-update 0 0)
                    (sut/GreedyStrategy-update 1 0)))))))
  (testing "update tracks counts"
    (let [s (sut/GreedyStrategy-update (sut/make-GreedyStrategy 3) 0 1)]
      (is (approx [1.0 0.0 0.0] (:counts s))))))

(deftest test-fn-test-mab-strategies
  (is (nil? (sut/test-mab-strategies))))

(deftest test-fn-demo-stub
  (is (= :UnsupportedOperation
         (:type (try (sut/demo-mab-strategies)
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
