(ns clojure-algorithms.machine-learning.mab
  "Multi-Armed Bandit (MAB) is a problem in reinforcement learning where an agent must\nlearn to choose the best action from a set of actions to maximize its reward.\n\nlearn more here: https://en.wikipedia.org/wiki/Multi-armed_bandit\n\n\nThe MAB problem can be described as follows:\n- There are N arms, each with a different probability of giving a reward.\n- The agent must learn to choose the best arm to pull in order to maximize its reward.\n\nHere 3 optimising strategies have been implemented:\n- Epsilon-Greedy\n- Upper Confidence Bound (UCB)\n- Thompson Sampling\n\nThere are two other strategies implemented to show the performance of\nthe optimising strategies:\n- Random strategy (full exploration)\n- Greedy strategy (full exploitation)\n\nThe performance of the strategies is evaluated by the cumulative reward\nover a number of rounds."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare test-mab-strategies demo-mab-strategies make-Bandit Bandit-pull make-Strategy poly-select-arm Strategy-select-arm poly-update Strategy-update make-EpsilonGreedy EpsilonGreedy-select-arm EpsilonGreedy-update make-UCB UCB-select-arm UCB-update make-ThompsonSampling ThompsonSampling-select-arm ThompsonSampling-update make-RandomStrategy RandomStrategy-select-arm RandomStrategy-update make-GreedyStrategy GreedyStrategy-select-arm GreedyStrategy-update)

(defrecord Bandit [probabilities num-arms])

(defrecord Strategy [])

(defrecord EpsilonGreedy [epsilon num-arms counts values])

(defrecord UCB [num-arms counts values total-counts])

(defrecord ThompsonSampling [num-arms successes failures])

(defrecord RandomStrategy [num-arms])

(defrecord GreedyStrategy [num-arms counts values])

(defn make-Bandit "Constructor for Bandit. A class to represent a multi-armed bandit."
  [probabilities] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:probabilities] probabilities))) (swap! self (fn [__c] (py-assoc-in __c [:num-arms] (count probabilities)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->Bandit @self)))

(defn make-Strategy
  "Constructor for Strategy."
  ([] (map->Strategy {})))

(defn make-EpsilonGreedy "Constructor for EpsilonGreedy. A class for a simple implementation of the Epsilon-Greedy strategy.\nFollow this link to learn more:\nhttps://medium.com/analytics-vidhya/the-epsilon-greedy-algorithm-for-reinforcement-learning-5fe6f96dc870"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-UCB "Constructor for UCB. A class for the Upper Confidence Bound (UCB) strategy.\nFollow this link to learn more:\nhttps://people.maths.bris.ac.uk/~maajg/teaching/stochopt/ucb.pdf"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-ThompsonSampling "Constructor for ThompsonSampling. A class for the Thompson Sampling strategy.\nFollow this link to learn more:\nhttps://en.wikipedia.org/wiki/Thompson_sampling"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-RandomStrategy "Constructor for RandomStrategy. A class for choosing an arm uniformly at random at each round to give\na better comparison with the other optimised strategies."
  [num-arms] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:num-arms] num-arms))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->RandomStrategy @self)))

(defn make-GreedyStrategy "Constructor for GreedyStrategy. A class for the Greedy strategy to show how full exploitation can be\ndetrimental to the performance of the strategy."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmulti poly-select-arm (fn [s & _] (class (if (instance? clojure.lang.Atom s) @s s))))

(defmulti poly-update (fn [s & _] (class (if (instance? clojure.lang.Atom s) @s s))))

(defn Bandit-pull "Pull an arm of the bandit.\n\nArgs:\n    arm_index: The arm to pull.\n\nReturns:\n    The reward for the arm.\n\nExample:\n    >>> bandit = Bandit([0.1, 0.5, 0.9])\n    >>> isinstance(bandit.pull(0), int)\n    True"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmethod poly-select-arm Strategy
  ([self] nil nil))

(defmethod poly-update Strategy
  ([self arm-index reward] nil nil))

(defmethod poly-select-arm EpsilonGreedy [& _] (throw (ex-info "requires np,numpy-attr:argmax" {:type :UnsupportedOperation})))

(defmethod poly-update EpsilonGreedy
  ([self arm-index reward] (let [n (atom nil)] (swap! self (fn [__c] (py-assoc-in __c [:counts arm-index] (py-add (py-get-in __c [:counts arm-index]) 1)))) (reset! n (py-get (:counts @self) arm-index)) (swap! self (fn [__c] (py-assoc-in __c [:values arm-index] (py-add (py-get-in __c [:values arm-index]) (py-div (py-sub reward (py-get (:values @self) arm-index)) @n))))) nil)))

(defmethod poly-select-arm UCB [& _] (throw (ex-info "requires np,numpy-attr:argmax" {:type :UnsupportedOperation})))

(defmethod poly-update UCB
  ([self arm-index reward] (let [n (atom nil)] (swap! self (fn [__c] (py-assoc-in __c [:counts arm-index] (py-add (py-get-in __c [:counts arm-index]) 1)))) (swap! self (fn [__c] (py-assoc-in __c [:total-counts] (py-add (py-get-in __c [:total-counts]) 1)))) (reset! n (py-get (:counts @self) arm-index)) (swap! self (fn [__c] (py-assoc-in __c [:values arm-index] (py-add (py-get-in __c [:values arm-index]) (py-div (py-sub reward (py-get (:values @self) arm-index)) @n))))) nil)))

(defmethod poly-select-arm ThompsonSampling [& _] (throw (ex-info "requires np,numpy-attr:argmax" {:type :UnsupportedOperation})))

(defmethod poly-update ThompsonSampling
  ([self arm-index reward] (if (py-truthy (py-eq reward 1)) (do (swap! self (fn [__c] (py-assoc-in __c [:successes arm-index] (py-add (py-get-in __c [:successes arm-index]) 1))))) (do (swap! self (fn [__c] (py-assoc-in __c [:failures arm-index] (py-add (py-get-in __c [:failures arm-index]) 1)))))) nil))

(defmethod poly-select-arm RandomStrategy [& _] (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmethod poly-update RandomStrategy
  ([self arm-index reward] nil nil))

(defmethod poly-select-arm GreedyStrategy [& _] (throw (ex-info "requires np,numpy-attr:argmax" {:type :UnsupportedOperation})))

(defmethod poly-update GreedyStrategy
  ([self arm-index reward] (let [n (atom nil)] (swap! self (fn [__c] (py-assoc-in __c [:counts arm-index] (py-add (py-get-in __c [:counts arm-index]) 1)))) (reset! n (py-get (:counts @self) arm-index)) (swap! self (fn [__c] (py-assoc-in __c [:values arm-index] (py-add (py-get-in __c [:values arm-index]) (py-div (py-sub reward (py-get (:values @self) arm-index)) @n))))) nil)))

(def Strategy-select-arm poly-select-arm)

(def EpsilonGreedy-select-arm poly-select-arm)

(def UCB-select-arm poly-select-arm)

(def ThompsonSampling-select-arm poly-select-arm)

(def RandomStrategy-select-arm poly-select-arm)

(def GreedyStrategy-select-arm poly-select-arm)

(def Strategy-update poly-update)

(def EpsilonGreedy-update poly-update)

(def UCB-update poly-update)

(def ThompsonSampling-update poly-update)

(def RandomStrategy-update poly-update)

(def GreedyStrategy-update poly-update)

(defn test-mab-strategies "Deterministic behavioural tests for the MAB strategies.\n\nThese checks feed each strategy a fixed sequence of rewards and assert\non the resulting internal state and arm selection, so a regression in\nthe update/select logic will fail the suite instead of only being\nvisible in the (stochastic) plotted demo."
  [] (let [--v (atom nil) arm (atom nil) epsilon-greedy (atom nil) first-round-arms (atom nil) greedy (atom nil) num-arms (atom nil) random-strategy (atom nil) selections (atom nil) thompson (atom nil) ucb (atom nil)] (reset! num-arms 3) (reset! greedy (make-GreedyStrategy nil)) (doseq [__i1 (py-iter (range 10))] (reset! --v __i1) (swap! greedy py-update 2)) (swap! greedy py-update 0) (swap! greedy py-update 1) (assert (py-truthy (py-eq (poly-select-arm greedy) 2))) (reset! epsilon-greedy (make-EpsilonGreedy nil nil)) (doseq [__i2 (py-iter (range 10))] (reset! --v __i2) (swap! epsilon-greedy py-update 1)) (swap! epsilon-greedy py-update 0) (swap! epsilon-greedy py-update 2) (assert (py-truthy (py-eq (poly-select-arm epsilon-greedy) 1))) (reset! ucb (make-UCB nil)) (reset! first-round-arms (py-set [])) (doseq [__i3 (py-iter (range @num-arms))] (reset! --v __i3) (reset! arm (poly-select-arm ucb)) (swap! first-round-arms (fn [__c] (conj (set __c) @arm))) (swap! ucb py-update @arm)) (assert (py-truthy (py-eq @first-round-arms (py-set (range @num-arms))))) (reset! thompson (make-ThompsonSampling nil)) (doseq [__i4 (py-iter (range 20))] (reset! --v __i4) (swap! thompson py-update 0) (swap! thompson py-update 1) (swap! thompson py-update 2)) (reset! selections (vec (for [--v (py-iter (range 50))] (poly-select-arm thompson)))) (assert (py-truthy (py-gt (py-count @selections 0) (py-floordiv (count @selections) 2)))) (reset! random-strategy (make-RandomStrategy nil)) (swap! random-strategy py-update 0) (assert (py-truthy (let [__c5 0 __c6 (poly-select-arm random-strategy) __c7 @num-arms] (and (py-le __c5 __c6) (py-lt __c6 __c7))))) nil))

(defn demo-mab-strategies "Run a stochastic simulation of the MAB strategies and plot their\ncumulative reward over time for visual comparison."
  [& _args]
  ;; stub: original requires plt; pure-Clojure port not applicable
  (throw (ex-info "requires plt" {:type :UnsupportedOperation})))

;; NOTE: Bandit.pull stubbed (np)
;; NOTE: EpsilonGreedy __init__ stubbed (np)
;; NOTE: EpsilonGreedy.select_arm stubbed (np,numpy-attr:argmax)
;; NOTE: UCB __init__ stubbed (np)
;; NOTE: UCB.select_arm stubbed (np,numpy-attr:argmax)
;; NOTE: ThompsonSampling __init__ stubbed (np)
;; NOTE: ThompsonSampling.select_arm stubbed (np,numpy-attr:argmax)
;; NOTE: RandomStrategy.select_arm stubbed (np)
;; NOTE: GreedyStrategy __init__ stubbed (np)
;; NOTE: GreedyStrategy.select_arm stubbed (np,numpy-attr:argmax)
;; NOTE: demo_mab_strategies stubbed (plt)
