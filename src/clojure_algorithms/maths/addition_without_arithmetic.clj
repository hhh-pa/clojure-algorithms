(ns clojure-algorithms.maths.addition-without-arithmetic
  "Illustrate how to add the integer without arithmetic operation\nAuthor: suraj Kumar\nTime Complexity: 1\nhttps://en.wikipedia.org/wiki/Bitwise_operation"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare add)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-band py-bxor)

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

(defn- py-band [a b]
  (cond (and (boolean? a) (boolean? b)) (and a b)
        (and (set? a) (set? b)) (clojure.set/intersection a b)
        (and (integer? (py-num-bool a)) (integer? (py-num-bool b)))
        (.and (.toBigInteger (bigint (py-num-bool a)))
              (.toBigInteger (bigint (py-num-bool b))))
        (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :band a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :band-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for &" {:type :TypeError})) r))
        :else (throw (ex-info "unsupported operand for &" {:type :TypeError}))))

(defn- py-bxor [a b]
  (cond (and (boolean? a) (boolean? b)) (not= a b)
        (and (set? a) (set? b))
        (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))
        (and (integer? (py-num-bool a)) (integer? (py-num-bool b)))
        (.xor (.toBigInteger (bigint (py-num-bool a)))
              (.toBigInteger (bigint (py-num-bool b))))
        (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :bxor a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for ^" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :bxor-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for ^" {:type :TypeError})) r))
        :else (throw (ex-info "unsupported operand for ^" {:type :TypeError}))))

(defn add "Add two integers using bitwise operations instead of arithmetic operators.\n- XOR (^) to add bits without carrying\n- AND (&) to calculate carry bits\n- Left shift (<<) to move the carry to the correct position\n\nExamples:\n>>> add(3, 5)\n8\n>>> add(13, 5)\n18\n>>> add(-7, 2)\n-5\n>>> add(0, -7)\n-7\n>>> add(-321, 1)\n-320"
  [first-v second-v] (let [first-v (atom first-v) second-v (atom second-v)] (let [c (atom nil)] (try (while (py-truthy (not (py-eq @second-v 0))) (reset! c (py-band @first-v @second-v)) (swap! first-v py-bxor @second-v) (reset! second-v (.shiftLeft (.toBigInteger (bigint @c)) (int 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @first-v})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
