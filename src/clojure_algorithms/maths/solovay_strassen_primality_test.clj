(ns clojure-algorithms.maths.solovay-strassen-primality-test
  "This script implements the Solovay-Strassen Primality test.\n\nThis probabilistic primality test is based on Euler's criterion. It is similar\nto the Fermat test but uses quadratic residues. It can quickly identify\ncomposite numbers but may occasionally classify composite numbers as prime.\n\nMore details and concepts about this can be found on:\nhttps://en.wikipedia.org/wiki/Solovay%E2%80%93Strassen_primality_test"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare jacobi-symbol solovay-strassen)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-sub py-floordiv py-mod py-pow py-powmod py-neg py-rng py-rand py-randint)

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

(defn- py-powmod [a b m]
  (let [a (py-num-bool a) b (py-num-bool b) m (py-num-bool m)]
    (when (or (not (integer? a)) (not (integer? b)) (not (integer? m)))
      (throw (ex-info "pow() 3rd argument not allowed unless all arguments are integers" {:type :TypeError})))
    (when (zero? m) (throw (ex-info "pow() 3rd argument cannot be 0" {:type :ValueError})))
    (let [bi (biginteger a) e (long b) mm (biginteger m)]
      (bigint (if (neg? b)
                (.modPow (.modInverse bi mm) (java.math.BigInteger/valueOf (- e)) mm)
                (.modPow bi (java.math.BigInteger/valueOf e) mm))))))

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(def ^:private py-rng (atom (java.util.Random.)))

(defn- py-rand [] (.nextDouble ^java.util.Random @py-rng))

(defn- py-randint [a b]
  (let [a (long a) b (long b) n (inc (- b a))]
    (if (<= n 0) (throw (ex-info "empty range" {:type :ValueError}))
        (if (<= n Integer/MAX_VALUE) (+ a (.nextInt ^java.util.Random @py-rng (int n)))
            (+ a (long (* (py-rand) n)))))))

(defn jacobi-symbol "Calculate the Jacobi symbol. The Jacobi symbol is a generalization\nof the Legendre symbol, which can be used to simplify computations involving\nquadratic residues. The Jacobi symbol is used in primality tests, like the\nSolovay-Strassen test, because it helps determine if an integer is a\nquadratic residue modulo a given modulus, providing valuable information\nabout the number's potential primality or compositeness.\n\nParameters:\n    random_a: A randomly chosen integer from 2 to n-2 (inclusive)\n    number: The number that is tested for primality\n\nReturns:\n    jacobi_symbol: The Jacobi symbol is a mathematical function\n    used to determine whether an integer is a quadratic residue modulo\n    another integer (usually prime) or not.\n\n>>> jacobi_symbol(2, 13)\n-1\n>>> jacobi_symbol(5, 19)\n1\n>>> jacobi_symbol(7, 14)\n0"
  [random-a number] (let [random-a (atom random-a) number (atom number)] (let [r (atom nil) t (atom nil)] (try (when (py-truthy (py-in @random-a [0 1])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @random-a}))) (swap! random-a py-mod @number) (reset! t 1) (while (py-truthy (not (py-eq @random-a 0))) (while (py-truthy (py-eq (py-mod @random-a 2) 0)) (swap! random-a py-floordiv 2) (reset! r (py-mod @number 8)) (when (py-truthy (py-in @r [3 5])) (reset! t (py-neg @t)))) (let [__d1 (vec [@number @random-a])] (reset! random-a (nth __d1 0)) (reset! number (nth __d1 1))) (when (py-truthy (let [__c2 (py-mod @random-a 4) __c3 (py-mod @number 4) __c4 3] (and (py-eq __c2 __c3) (py-eq __c3 __c4)))) (reset! t (py-neg @t))) (swap! random-a py-mod @number)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-eq @number 1)) @t 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn solovay-strassen "Check whether the input number is prime or not using\nthe Solovay-Strassen Primality test\n\nParameters:\n    number: The number that is tested for primality\n    iterations: The number of times that the test is run\n    which effects the accuracy\n\nReturns:\n    result: True if number is probably prime and false\n    if not\n\n>>> random.seed(10)\n>>> solovay_strassen(13, 5)\nTrue\n>>> solovay_strassen(9, 10)\nFalse\n>>> solovay_strassen(17, 15)\nTrue"
  [number iterations] (let [--v (atom nil) a (atom nil) x (atom nil) y (atom nil)] (try (when (py-truthy (py-le number 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (py-le number 3)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (doseq [__i1 (py-iter (range iterations))] (reset! --v __i1) (reset! a (py-randint 2 (py-sub number 2))) (reset! x (jacobi-symbol @a number)) (reset! y (py-powmod @a (py-floordiv (py-sub number 1) 2) number)) (when (py-truthy (let [__b2 (py-eq @x 0)] (if (py-truthy __b2) __b2 (not (py-eq @y (py-mod @x number)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
