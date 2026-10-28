(ns clojure-algorithms.maths.power-using-recursion
  "== Raise base to the power of exponent using recursion ==\n    Input -->\n        Enter the base: 3\n        Enter the exponent: 4\n    Output  -->\n        3 to the power of 4 is 81\n    Input -->\n        Enter the base: 2\n        Enter the exponent: 0\n    Output -->\n        2 to the power of 0 is 1"
  (:require [clojure.set :as set]))

(declare power)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-sub py-repeat-vec py-mul)

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

(defn power "Calculate the power of a base raised to an exponent.\n\n>>> power(3, 4)\n81\n>>> power(2, 0)\n1\n>>> all(power(base, exponent) == pow(base, exponent)\n...     for base in range(-10, 10) for exponent in range(10))\nTrue\n>>> power('a', 1)\n'a'\n>>> power('a', 2)\nTraceback (most recent call last):\n    ...\nTypeError: can't multiply sequence by non-int of type 'str'\n>>> power('a', 'b')\nTraceback (most recent call last):\n    ...\nTypeError: unsupported operand type(s) for -: 'str' and 'int'\n>>> power(2, -1)\nTraceback (most recent call last):\n    ...\nRecursionError: maximum recursion depth exceeded\n>>> power(0, 0)\n1\n>>> power(0, 1)\n0\n>>> power(5,6)\n15625\n>>> power(23, 12)\n21914624432020321"
  [base exponent] (if (py-truthy exponent) (py-mul base (power base (py-sub exponent 1))) 1))
