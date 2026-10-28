(ns clojure-algorithms.maths.binary-multiplication
  "Binary Multiplication\nThis is a method to find a*b in a time complexity of O(log b)\nThis is one of the most commonly used methods of finding result of multiplication.\nAlso useful in cases where solution to (a*b)%c is required,\nwhere a,b,c can be numbers over the computers calculation limits.\nDone using iteration, can also be done using recursion\n\nLet's say you need to calculate a * b\nRULE 1 : a * b = (a+a) * (b/2) ---- example : 4 * 4 = (4+4) * (4/2) = 8 * 2\nRULE 2 : IF b is odd, then ---- a * b = a + (a * (b - 1)), where (b - 1) is even.\nOnce b is even, repeat the process to get a * b\nRepeat the process until b = 1 or b = 0, because a*1 = a and a*0 = 0\n\nAs far as the modulo is concerned,\nthe fact : (a+b) % c = ((a%c) + (b%c)) % c\nNow apply RULE 1 or 2, whichever is required.\n\n@author chinmoy159"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare binary-multiply binary-mod-multiply)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-add py-mod py-band)

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

(defn binary-multiply "Multiply 'a' and 'b' using bitwise multiplication.\n\nParameters:\na (int): The first number.\nb (int): The second number.\n\nReturns:\nint: a * b\n\nExamples:\n>>> binary_multiply(2, 3)\n6\n>>> binary_multiply(5, 0)\n0\n>>> binary_multiply(3, 4)\n12\n>>> binary_multiply(10, 5)\n50\n>>> binary_multiply(0, 5)\n0\n>>> binary_multiply(2, 1)\n2\n>>> binary_multiply(1, 10)\n10"
  [a b] (let [a (atom a) b (atom b)] (let [res (atom nil)] (try (reset! res 0) (while (py-truthy (py-gt @b 0)) (when (py-truthy (py-band @b 1)) (swap! res py-add @a)) (swap! a py-add @a) (swap! b (fn [__o] (.shiftRight (.toBigInteger (bigint __o)) (int 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn binary-mod-multiply "Calculate (a * b) % c using binary multiplication and modular arithmetic.\n\nParameters:\na (int): The first number.\nb (int): The second number.\nmodulus (int): The modulus.\n\nReturns:\nint: (a * b) % modulus.\n\nExamples:\n>>> binary_mod_multiply(2, 3, 5)\n1\n>>> binary_mod_multiply(5, 0, 7)\n0\n>>> binary_mod_multiply(3, 4, 6)\n0\n>>> binary_mod_multiply(10, 5, 13)\n11\n>>> binary_mod_multiply(2, 1, 5)\n2\n>>> binary_mod_multiply(1, 10, 3)\n1"
  [a b modulus] (let [a (atom a) b (atom b)] (let [res (atom nil)] (try (reset! res 0) (while (py-truthy (py-gt @b 0)) (when (py-truthy (py-band @b 1)) (reset! res (py-mod (py-add (py-mod @res modulus) (py-mod @a modulus)) modulus))) (swap! a py-add @a) (swap! b (fn [__o] (.shiftRight (.toBigInteger (bigint __o)) (int 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
