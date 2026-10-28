(ns clojure-algorithms.machine-learning.q-learning-test
  (:require [clojure-algorithms.machine-learning.q-learning :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]))

(use-fixtures :each (fn [t]
                      (reset! sut/q-table {})
                      (reset! sut/EPSILON 0.2)
                      (reset! sut/current-state [0 0])
                      (t)
                      (reset! sut/q-table {})
                      (reset! sut/EPSILON 0.2)
                      (reset! sut/current-state [0 0])))

(deftest test-fn-get-q-value-examples
  (is (approx 0.0 (sut/get-q-value [0 0] 2)))
  (testing "reads install the 0.0 default like the upstream defaultdict"
    (reset! sut/q-table {})
    (sut/get-q-value [1 1] 0)
    (is (= {[1 1] {0 0.0}} @sut/q-table))))

(deftest test-fn-get-best-action-examples
  (swap! sut/q-table assoc-in [[0 0] 1] 0.7)
  (swap! sut/q-table assoc-in [[0 0] 2] 0.7)
  (swap! sut/q-table assoc-in [[0 0] 3] 0.5)
  (is (contains? #{1 2} (sut/get-best-action [0 0] [1 2 3])))
  (is (thrown? Exception (sut/get-best-action [0 0] [])))
  (testing "unique maximum is always picked"
    (swap! sut/q-table assoc-in [[0 0] 3] 0.9)
    (is (= 3 (sut/get-best-action [0 0] [1 2 3])))))

(deftest test-fn-choose-action-examples
  (reset! sut/EPSILON 0.0)
  (swap! sut/q-table assoc-in [[0 0] 1] 1.0)
  (swap! sut/q-table assoc-in [[0 0] 2] 0.5)
  (is (= 1 (sut/choose-action [0 0] [1 2])))
  (is (thrown? Exception (sut/choose-action [0 0] [])))
  (testing "full exploration still returns a valid action"
    (reset! sut/EPSILON 1.0)
    (is (contains? #{1 2} (sut/choose-action [0 0] [1 2])))))

(deftest test-fn-update-examples
  (sut/update [0 0] 1 1.0 [0 1] [1 2] true 0.5 0.9)
  (is (approx 0.5 (sut/get-q-value [0 0] 1)))
  (testing "bootstrapping uses the discounted best next value"
    (reset! sut/q-table {})
    (swap! sut/q-table assoc-in [[0 1] 2] 2.0)
    (sut/update [0 0] 1 1.0 [0 1] [1 2] false 0.5 0.9)
    (is (approx 1.4 (sut/get-q-value [0 0] 1))))
  (testing "defaults come from the module constants"
    (reset! sut/q-table {})
    (sut/update [0 0] 1 1.0 [0 1] [1 2] true)
    (is (approx (* 0.1 1.0) (sut/get-q-value [0 0] 1)))))

(deftest test-fn-get-policy-examples
  (swap! sut/q-table assoc-in [[1 2] 1] 2.0)
  (swap! sut/q-table assoc-in [[1 2] 2] 1.0)
  (is (= 1 (get (sut/get-policy) [1 2])))
  (testing "states without actions are skipped"
    (swap! sut/q-table assoc [9 9] {})
    (is (not (contains? (sut/get-policy) [9 9])))))

(deftest test-fn-reset-env-examples
  (reset! sut/current-state [1 1])
  (is (= [0 0] (sut/reset-env)))
  (is (= [0 0] @sut/current-state)))

(deftest test-fn-step-env-examples
  (testing "moves, walls, rewards and termination"
    (is (= [[0 1] -1.0 false] (sut/step-env 1)))
    (is (= [[0 0] -1.0 false] (sut/step-env 3)))
    (is (= [[0 0] -1.0 false] (sut/step-env 0)))
    (reset! sut/current-state [3 2])
    (is (= [[3 3] 10.0 true] (sut/step-env 1))))
  (testing "unknown actions leave the state unchanged"
    (reset! sut/current-state [1 1])
    (is (= [[1 1] -1.0 false] (sut/step-env 99)))))

(deftest test-fn-run-q-learning-smoke
  (let [out (with-out-str (sut/run-q-learning))]
    (is (str/includes? out "Learned Policy (state: action):"))
    (is (seq @sut/q-table))
    (is (< @sut/EPSILON 0.2))))
