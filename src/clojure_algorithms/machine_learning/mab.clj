(ns clojure-algorithms.machine-learning.mab
  "Multi-Armed Bandit (MAB) is a problem in reinforcement learning where an agent must\nlearn to choose the best action from a set of actions to maximize its reward.\n\nlearn more here: https://en.wikipedia.org/wiki/Multi-armed_bandit\n\n\nThe MAB problem can be described as follows:\n- There are N arms, each with a different probability of giving a reward.\n- The agent must learn to choose the best arm to pull in order to maximize its reward.\n\nHere 3 optimising strategies have been implemented:\n- Epsilon-Greedy\n- Upper Confidence Bound (UCB)\n- Thompson Sampling\n\nThere are two other strategies implemented to show the performance of\nthe optimising strategies:\n- Random strategy (full exploration)\n- Greedy strategy (full exploitation)\n\nThe performance of the strategies is evaluated by the cumulative reward\nover a number of rounds."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare test-mab-strategies demo-mab-strategies make-Bandit Bandit-pull make-Strategy poly-select-arm Strategy-select-arm poly-update Strategy-update make-EpsilonGreedy EpsilonGreedy-select-arm EpsilonGreedy-update make-UCB UCB-select-arm UCB-update make-ThompsonSampling ThompsonSampling-select-arm ThompsonSampling-update make-RandomStrategy RandomStrategy-select-arm RandomStrategy-update make-GreedyStrategy GreedyStrategy-select-arm GreedyStrategy-update)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-div py-floordiv py-set py-dict-update py-update py-count py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-floordiv [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :floordiv a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for //" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :floordiv-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for //" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for //" {:type :TypeError})))
                (when (zero? b)
                  (throw (ex-info "integer division by zero" {:type :ZeroDivisionError})))
                (let [r (/ (rationalize a) (rationalize b))]
                  (if (integer? r)
                    (if (or (float? a) (float? b)) (double r) r)
                    (let [n (numerator r) d (denominator r)
                          q (quot n d)
                          qq (if (and (not (zero? (rem n d))) (neg? n)) (dec q) q)]
                      (if (or (float? a) (float? b)) (double qq) qq)))))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-dict-update [d x extra]
  (let [d (if (map? d) d (throw (ex-info "update on non-dict" {:type :AttributeError})))]
    (merge d (cond (nil? x) {}
                   (map? x) x
                   (sequential? x) (into {} (map (fn [p] [(nth p 0) (nth p 1)]) x))
                   :else (throw (ex-info "cannot convert to dict" {:type :TypeError})))
           extra)))

(defn- py-update [c x]
  (cond (map? c) (py-dict-update c x {})
        (set? c) (clojure.set/union c (py-set x))
        :else (throw (ex-info "no attribute 'update'" {:type :AttributeError}))))

(defn- py-count [c x]
  (cond (and (string? c) (string? x))
        (if (empty? x) (inc (count c))
            (loop [n 0 i 0]
              (let [j (.indexOf ^String c ^String x (int i))]
                (if (neg? j) n (recur (inc n) (+ j (count x)))))))
        (sequential? c) (count (filter #(py-eq % x) c))
        :else (throw (ex-info "no attribute 'count'" {:type :AttributeError}))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

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
