(ns clojure-algorithms.maths.joint-probability-distribution
  "Calculate joint probability distribution\nhttps://en.wikipedia.org/wiki/Joint_probability_distribution"
  (:require [clojure.set :as set]))

(declare joint-probability-distribution expectation variance covariance standard-deviation)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-sub py-repeat-vec py-mul py-pow)

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

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

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

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn joint-probability-distribution ">>> joint_distribution =  joint_probability_distribution(\n...     [1, 2], [-2, 5, 8], [0.7, 0.3], [0.3, 0.5, 0.2]\n... )\n>>> from math import isclose\n>>> isclose(joint_distribution.pop((1, 8)), 0.14)\nTrue\n>>> joint_distribution\n{(1, -2): 0.21, (1, 5): 0.35, (2, -2): 0.09, (2, 5): 0.15, (2, 8): 0.06}"
  [x-values y-values x-probabilities y-probabilities] (into {} (for [[x x-prob] (py-iter (map vector x-values x-probabilities)) [y y-prob] (py-iter (map vector y-values y-probabilities))] (list [[x y] (py-mul x-prob y-prob)]))))

(defn expectation ">>> from math import isclose\n>>> isclose(expectation([1, 2], [0.7, 0.3]), 1.3)\nTrue"
  [values probabilities] (reduce +' 0 (doall (for [[x p] (py-iter (map vector values probabilities))] (py-mul x p)))))

(defn variance ">>> from math import isclose\n>>> isclose(variance([1,2],[0.7,0.3]), 0.21)\nTrue"
  [values probabilities] (let [mean (atom nil)] (try (reset! mean (expectation values probabilities)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[x p] (py-iter (map vector values probabilities))] (py-mul (py-pow (py-sub x @mean) 2) p))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn covariance ">>> covariance([1, 2], [-2, 5, 8], [0.7, 0.3], [0.3, 0.5, 0.2])\n-2.7755575615628914e-17"
  [x-values y-values x-probabilities y-probabilities] (let [mean-x (atom nil) mean-y (atom nil)] (try (reset! mean-x (expectation x-values x-probabilities)) (reset! mean-y (expectation y-values y-probabilities)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (reduce +' 0 (doall (for [[x px] (py-iter (map vector x-values x-probabilities)) [y py] (py-iter (map vector y-values y-probabilities))] (py-mul (py-mul (py-mul (py-sub x @mean-x) (py-sub y @mean-y)) px) py))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn standard-deviation ">>> standard_deviation(0.21)\n0.458257569495584"
  [variance] (py-pow variance 0.5))
