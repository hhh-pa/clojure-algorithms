(ns clojure-algorithms.maths.mobius-function
  "References: https://en.wikipedia.org/wiki/M%C3%B6bius_function\nReferences: wikipedia:square free number\npsf/black : True\nruff : True"
  (:require [clojure-algorithms.maths.is-square-free :as m2] [clojure-algorithms.maths.prime-factors :as m1]))

(declare mobius)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-mod py-neg)

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

(defn- py-mod [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :mod a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for %" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :mod-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for %" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for %" {:type :TypeError})))
                (when (zero? b)
                  (throw (ex-info "integer modulo by zero" {:type :ZeroDivisionError})))
                (mod a b))))

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn mobius "Mobius function\n>>> mobius(24)\n0\n>>> mobius(-1)\n1\n>>> mobius('asd')\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'str'\n>>> mobius(10**400)\n0\n>>> mobius(10**-400)\n1\n>>> mobius(-1424)\n1\n>>> mobius([1, '2', 2.0])\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'list'"
  [n] (let [factors (atom nil)] (try (reset! factors (m1/prime-factors n)) (when (py-truthy (m2/is-square-free @factors)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-mod (count @factors) 2)) (py-neg 1) 1)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
