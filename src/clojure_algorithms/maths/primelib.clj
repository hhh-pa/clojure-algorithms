(ns clojure-algorithms.maths.primelib
  "Created on Thu Oct  5 16:44:23 2017\n\n@author: Christian Bender\n\nThis Python library contains some useful functions to deal with\nprime numbers and whole numbers.\n\nOverview:\n\nis_prime(number)\nsieve_er(N)\nget_prime_numbers(N)\nprime_factorization(number)\ngreatest_prime_factor(number)\nsmallest_prime_factor(number)\nget_prime(n)\nget_primes_between(pNumber1, pNumber2)\n\n----\n\nis_even(number)\nis_odd(number)\nkg_v(number1, number2)  // least common multiple\nget_divisors(number)    // all divisors of 'number' inclusive 1, number\nis_perfect_number(number)\n\nNEW-FUNCTIONS\n\nsimplify_fraction(numerator, denominator)\nfactorial (n) // n!\nfib (n) // calculate the n-th fibonacci term.\n\n-----\n\ngoldbach(number)  // Goldbach's assumption"
  (:require [clojure.string :as str] [clojure.set :as set] [clojure-algorithms.maths.greatest-common-divisor :as m1]))

(declare is-prime sieve-er get-prime-numbers prime-factorization greatest-prime-factor smallest-prime-factor is-even is-odd goldbach kg-v get-prime get-primes-between get-divisors is-perfect-number simplify-fraction factorial fib)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-slice py-add py-sub py-repeat-vec py-mul py-floordiv py-mod py-neg py-abs py-min py-min-by py-max py-max-by py-list py-count py-float-str py-quote py-repr py-str py-round)

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

(defn- py-slice [c lo hi step]
  (when (map? c) (throw (ex-info "unhashable slice" {:type :TypeError})))
  (let [s (vec (seq c)) n (count s)
        step (if (nil? step) 1 step)]
    (when (zero? step) (throw (ex-info "slice step cannot be zero" {:type :ValueError})))
    (let [idxs (if (pos? step)
                 (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                        (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                        step)
                 (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                        (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                        step))
          res (mapv s idxs)]
      (if (string? c) (apply str res) res))))

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

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

(defn- py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

(defn is-prime "input: positive integer 'number'\nreturns true if 'number' is prime otherwise false.\n\n>>> is_prime(3)\nTrue\n>>> is_prime(10)\nFalse\n>>> is_prime(97)\nTrue\n>>> is_prime(9991)\nFalse\n>>> is_prime(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and positive\n>>> is_prime(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and positive"
  [number] (let [divisor (atom nil) status (atom nil)] (try (assert (py-truthy (let [__b1 (integer? number)] (if (py-truthy __b1) (py-ge number 0) __b1))) (py-str "'number' must been an int and positive")) (reset! status true) (when (py-truthy (py-le number 1)) (reset! status false)) (try (doseq [__i2 (py-iter (range 2 (py-add (py-round (Math/sqrt number)) 1)))] (reset! divisor __i2) (when (py-truthy (py-eq (py-mod number @divisor) 0)) (reset! status false) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (assert (py-truthy (boolean? @status)) (py-str "'status' must been from type bool")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @status})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn sieve-er "input: positive integer 'N' > 2\nreturns a list of prime numbers from 2 up to N.\n\nThis function implements the algorithm called\nsieve of erathostenes.\n\n>>> sieve_er(8)\n[2, 3, 5, 7]\n>>> sieve_er(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'N' must been an int and > 2\n>>> sieve_er(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'N' must been an int and > 2"
  [n] (let [ans (atom nil) begin-list (atom nil) i (atom nil) j (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-gt n 2) __b1))) (py-str "'N' must been an int and > 2")) (reset! begin-list (py-list (range 2 (py-add n 1)))) (reset! ans []) (doseq [__i2 (py-iter (range (count @begin-list)))] (reset! i __i2) (doseq [__i3 (py-iter (range (py-add @i 1) (count @begin-list)))] (reset! j __i3) (when (py-truthy (let [__b4 (not (py-eq (py-get @begin-list @i) 0))] (if (py-truthy __b4) (py-eq (py-mod (py-get @begin-list @j) (py-get @begin-list @i)) 0) __b4))) (swap! begin-list (fn [__c] (py-assoc-in __c [@j] 0)))))) (reset! ans (vec (for [x (py-iter @begin-list) :when (py-truthy (not (py-eq x 0)))] x))) (assert (py-truthy (sequential? @ans)) (py-str "'ans' must been from type list")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-prime-numbers "input: positive integer 'N' > 2\nreturns a list of prime numbers from 2 up to N (inclusive)\nThis function is more efficient as function 'sieveEr(...)'\n\n>>> get_prime_numbers(8)\n[2, 3, 5, 7]\n>>> get_prime_numbers(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'N' must been an int and > 2\n>>> get_prime_numbers(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'N' must been an int and > 2"
  [n] (let [ans (atom nil) number (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-gt n 2) __b1))) (py-str "'N' must been an int and > 2")) (reset! ans []) (doseq [__i2 (py-iter (range 2 (py-add n 1)))] (reset! number __i2) (when (py-truthy (is-prime @number)) (swap! ans conj @number))) (assert (py-truthy (sequential? @ans)) (py-str "'ans' must been from type list")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn prime-factorization "input: positive integer 'number'\nreturns a list of the prime number factors of 'number'\n\n>>> prime_factorization(0)\n[0]\n>>> prime_factorization(8)\n[2, 2, 2]\n>>> prime_factorization(287)\n[7, 41]\n>>> prime_factorization(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0\n>>> prime_factorization(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0"
  [number] (let [ans (atom nil) factor (atom nil) quotient (atom nil)] (try (assert (py-truthy (let [__b1 (integer? number)] (if (py-truthy __b1) (py-ge number 0) __b1))) (py-str "'number' must been an int and >= 0")) (reset! ans []) (reset! factor 2) (reset! quotient number) (if (py-truthy (py-in number (set [0 1]))) (do (swap! ans conj number)) (do (if (py-truthy (not (py-truthy (is-prime number)))) (do (while (py-truthy (not (py-eq @quotient 1))) (if (py-truthy (let [__b2 (is-prime @factor)] (if (py-truthy __b2) (py-eq (py-mod @quotient @factor) 0) __b2))) (do (swap! ans conj @factor) (swap! quotient py-floordiv @factor)) (do (swap! factor py-add 1))))) (do (swap! ans conj number))))) (assert (py-truthy (sequential? @ans)) (py-str "'ans' must been from type list")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn greatest-prime-factor "input: positive integer 'number' >= 0\nreturns the greatest prime number factor of 'number'\n\n>>> greatest_prime_factor(0)\n0\n>>> greatest_prime_factor(8)\n2\n>>> greatest_prime_factor(287)\n41\n>>> greatest_prime_factor(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0\n>>> greatest_prime_factor(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0"
  [number] (let [ans (atom nil) prime-factors (atom nil)] (try (assert (py-truthy (let [__b1 (integer? number)] (if (py-truthy __b1) (py-ge number 0) __b1))) (py-str "'number' must been an int and >= 0")) (reset! ans 0) (reset! prime-factors (prime-factorization number)) (reset! ans (py-max @prime-factors)) (assert (py-truthy (integer? @ans)) (py-str "'ans' must been from type int")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn smallest-prime-factor "input: integer 'number' >= 0\nreturns the smallest prime number factor of 'number'\n\n>>> smallest_prime_factor(0)\n0\n>>> smallest_prime_factor(8)\n2\n>>> smallest_prime_factor(287)\n7\n>>> smallest_prime_factor(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0\n>>> smallest_prime_factor(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 0"
  [number] (let [ans (atom nil) prime-factors (atom nil)] (try (assert (py-truthy (let [__b1 (integer? number)] (if (py-truthy __b1) (py-ge number 0) __b1))) (py-str "'number' must been an int and >= 0")) (reset! ans 0) (reset! prime-factors (prime-factorization number)) (reset! ans (py-min @prime-factors)) (assert (py-truthy (integer? @ans)) (py-str "'ans' must been from type int")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-even "input: integer 'number'\nreturns true if 'number' is even, otherwise false.\n\n>>> is_even(0)\nTrue\n>>> is_even(8)\nTrue\n>>> is_even(287)\nFalse\n>>> is_even(-1)\nFalse\n>>> is_even(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int"
  [number] (try (assert (py-truthy (integer? number)) (py-str "'number' must been an int")) (assert (py-truthy (boolean? (py-eq (py-mod number 2) 0))) (py-str "compare must been from type bool")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (py-mod number 2) 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn is-odd "input: integer 'number'\nreturns true if 'number' is odd, otherwise false.\n\n>>> is_odd(0)\nFalse\n>>> is_odd(8)\nFalse\n>>> is_odd(287)\nTrue\n>>> is_odd(-1)\nTrue\n>>> is_odd(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int"
  [number] (try (assert (py-truthy (integer? number)) (py-str "'number' must been an int")) (assert (py-truthy (boolean? (not (py-eq (py-mod number 2) 0)))) (py-str "compare must been from type bool")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (not (py-eq (py-mod number 2) 0))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn goldbach "Goldbach's assumption\ninput: a even positive integer 'number' > 2\nreturns a list of two prime numbers whose sum is equal to 'number'\n\n>>> goldbach(8)\n[3, 5]\n>>> goldbach(824)\n[3, 821]\n>>> goldbach(0)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int, even and > 2\n>>> goldbach(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int, even and > 2\n>>> goldbach(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int, even and > 2"
  [number] (let [ans (atom nil) i (atom nil) j (atom nil) len-pn (atom nil) loop-v (atom nil) prime-numbers (atom nil)] (try (assert (py-truthy (let [__b2 (integer? number)] (if (py-truthy __b2) (let [__b1 (py-gt number 2)] (if (py-truthy __b1) (is-even number) __b1)) __b2))) (py-str "'number' must been an int, even and > 2")) (reset! ans []) (reset! prime-numbers (get-prime-numbers number)) (reset! len-pn (count @prime-numbers)) (reset! i 0) (reset! j nil) (reset! loop-v true) (while (py-truthy (let [__b3 (py-lt @i @len-pn)] (if (py-truthy __b3) @loop-v __b3))) (reset! j (py-add @i 1)) (while (py-truthy (let [__b4 (py-lt @j @len-pn)] (if (py-truthy __b4) @loop-v __b4))) (when (py-truthy (py-eq (py-add (py-get @prime-numbers @i) (py-get @prime-numbers @j)) number)) (reset! loop-v false) (swap! ans conj (py-get @prime-numbers @i)) (swap! ans conj (py-get @prime-numbers @j))) (swap! j py-add 1)) (swap! i py-add 1)) (assert (py-truthy (let [__b8 (sequential? @ans)] (if (py-truthy __b8) (let [__b7 (py-eq (count @ans) 2)] (if (py-truthy __b7) (let [__b6 (py-eq (py-add (py-get @ans 0) (py-get @ans 1)) number)] (if (py-truthy __b6) (let [__b5 (is-prime (py-get @ans 0))] (if (py-truthy __b5) (is-prime (py-get @ans 1)) __b5)) __b6)) __b7)) __b8))) (py-str "'ans' must contains two primes. And sum of elements must been eq 'number'")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn kg-v "Least common multiple\ninput: two positive integer 'number1' and 'number2'\nreturns the least common multiple of 'number1' and 'number2'\n\n>>> kg_v(8,10)\n40\n>>> kg_v(824,67)\n55208\n>>> kg_v(1, 10)\n10\n>>> kg_v(0)\nTraceback (most recent call last):\n    ...\nTypeError: kg_v() missing 1 required positional argument: 'number2'\n>>> kg_v(10,-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number1' and 'number2' must been positive integer.\n>>> kg_v(\"test\",\"test2\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number1' and 'number2' must been positive integer."
  [number1 number2] (let [--v (atom nil) ans (atom nil) count1 (atom nil) count2 (atom nil) done (atom nil) n (atom nil) prime-fac-1 (atom nil) prime-fac-2 (atom nil)] (try (assert (py-truthy (let [__b3 (integer? number1)] (if (py-truthy __b3) (let [__b2 (integer? number2)] (if (py-truthy __b2) (let [__b1 (py-ge number1 1)] (if (py-truthy __b1) (py-ge number2 1) __b1)) __b2)) __b3))) (py-str "'number1' and 'number2' must been positive integer.")) (reset! ans 1) (if (py-truthy (let [__b4 (py-gt number1 1)] (if (py-truthy __b4) (py-gt number2 1) __b4))) (do (reset! prime-fac-1 (prime-factorization number1)) (reset! prime-fac-2 (prime-factorization number2))) (do (when (py-truthy (let [__b5 (py-eq number1 1)] (if (py-truthy __b5) __b5 (py-eq number2 1)))) (reset! prime-fac-1 []) (reset! prime-fac-2 []) (reset! ans (py-max [number1 number2]))))) (reset! count1 0) (reset! count2 0) (reset! done []) (doseq [__i6 (py-iter @prime-fac-1)] (reset! n __i6) (when (py-truthy (not (py-in @n @done))) (if (py-truthy (py-in @n @prime-fac-2)) (do (reset! count1 (py-count @prime-fac-1 @n)) (reset! count2 (py-count @prime-fac-2 @n)) (doseq [__i7 (py-iter (range (py-max [@count1 @count2])))] (reset! --v __i7) (swap! ans py-mul @n))) (do (reset! count1 (py-count @prime-fac-1 @n)) (doseq [__i8 (py-iter (range @count1))] (reset! --v __i8) (swap! ans py-mul @n)))) (swap! done conj @n))) (doseq [__i9 (py-iter @prime-fac-2)] (reset! n __i9) (when (py-truthy (not (py-in @n @done))) (reset! count2 (py-count @prime-fac-2 @n)) (doseq [__i10 (py-iter (range @count2))] (reset! --v __i10) (swap! ans py-mul @n)) (swap! done conj @n))) (assert (py-truthy (let [__b11 (integer? @ans)] (if (py-truthy __b11) (py-ge @ans 0) __b11))) (py-str "'ans' must been from type int and positive")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-prime "Gets the n-th prime number.\ninput: positive integer 'n' >= 0\nreturns the n-th prime number, beginning at index 0\n\n>>> get_prime(0)\n2\n>>> get_prime(8)\n23\n>>> get_prime(824)\n6337\n>>> get_prime(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been a positive int\n>>> get_prime(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been a positive int"
  [n] (let [ans (atom nil) index (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-ge n 0) __b1))) (py-str "'number' must been a positive int")) (reset! index 0) (reset! ans 2) (while (py-truthy (py-lt @index n)) (swap! index py-add 1) (swap! ans py-add 1) (while (py-truthy (not (py-truthy (is-prime @ans)))) (swap! ans py-add 1))) (assert (py-truthy (let [__b2 (integer? @ans)] (if (py-truthy __b2) (is-prime @ans) __b2))) (py-str "'ans' must been a prime number and from type int")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-primes-between "input: prime numbers 'pNumber1' and 'pNumber2'\n        pNumber1 < pNumber2\nreturns a list of all prime numbers between 'pNumber1' (exclusive)\n        and 'pNumber2' (exclusive)\n\n>>> get_primes_between(3, 67)\n[5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]\n>>> get_primes_between(0)\nTraceback (most recent call last):\n    ...\nTypeError: get_primes_between() missing 1 required positional argument: 'p_number_2'\n>>> get_primes_between(0, 1)\nTraceback (most recent call last):\n    ...\nAssertionError: The arguments must been prime numbers and 'pNumber1' < 'pNumber2'\n>>> get_primes_between(-1, 3)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and positive\n>>> get_primes_between(\"test\",\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and positive"
  [p-number-1 p-number-2] (let [ans (atom nil) number (atom nil)] (try (assert (py-truthy (let [__b2 (is-prime p-number-1)] (if (py-truthy __b2) (let [__b1 (is-prime p-number-2)] (if (py-truthy __b1) (py-lt p-number-1 p-number-2) __b1)) __b2))) (py-str "The arguments must been prime numbers and 'pNumber1' < 'pNumber2'")) (reset! number (py-add p-number-1 1)) (reset! ans []) (while (py-truthy (not (py-truthy (is-prime @number)))) (swap! number py-add 1)) (while (py-truthy (py-lt @number p-number-2)) (swap! ans conj @number) (swap! number py-add 1) (while (py-truthy (not (py-truthy (is-prime @number)))) (swap! number py-add 1))) (assert (py-truthy (let [__b4 (sequential? @ans)] (if (py-truthy __b4) (let [__b3 (not (py-eq (py-get @ans 0) p-number-1))] (if (py-truthy __b3) (not (py-eq (py-get @ans (py-sub (count @ans) 1)) p-number-2)) __b3)) __b4))) (py-str "'ans' must been a list without the arguments")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-divisors "input: positive integer 'n' >= 1\nreturns all divisors of n (inclusive 1 and 'n')\n\n>>> get_divisors(8)\n[1, 2, 4, 8]\n>>> get_divisors(824)\n[1, 2, 4, 8, 103, 206, 412, 824]\n>>> get_divisors(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'n' must been int and >= 1\n>>> get_divisors(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'n' must been int and >= 1"
  [n] (let [ans (atom nil) divisor (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-ge n 1) __b1))) (py-str "'n' must been int and >= 1")) (reset! ans []) (doseq [__i2 (py-iter (range 1 (py-add n 1)))] (reset! divisor __i2) (when (py-truthy (py-eq (py-mod n @divisor) 0)) (swap! ans conj @divisor))) (assert (py-truthy (let [__b3 (py-eq (py-get @ans 0) 1)] (if (py-truthy __b3) (py-eq (py-get @ans (py-sub (count @ans) 1)) n) __b3))) (py-str "Error in function getDivisiors(...)")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-perfect-number "input: positive integer 'number' > 1\nreturns true if 'number' is a perfect number otherwise false.\n\n>>> is_perfect_number(28)\nTrue\n>>> is_perfect_number(824)\nFalse\n>>> is_perfect_number(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 1\n>>> is_perfect_number(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'number' must been an int and >= 1"
  [number] (let [divisors (atom nil)] (try (assert (py-truthy (let [__b1 (integer? number)] (if (py-truthy __b1) (py-gt number 1) __b1))) (py-str "'number' must been an int and >= 1")) (reset! divisors (get-divisors number)) (assert (py-truthy (let [__b3 (sequential? @divisors)] (if (py-truthy __b3) (let [__b2 (py-eq (py-get @divisors 0) 1)] (if (py-truthy __b2) (py-eq (py-get @divisors (py-sub (count @divisors) 1)) number) __b2)) __b3))) (py-str "Error in help-function getDivisiors(...)")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (reduce +' 0 (py-slice @divisors nil (py-neg 1) nil)) number)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn simplify-fraction "input: two integer 'numerator' and 'denominator'\nassumes: 'denominator' != 0\nreturns: a tuple with simplify numerator and denominator.\n\n>>> simplify_fraction(10, 20)\n(1, 2)\n>>> simplify_fraction(10, -1)\n(10, -1)\n>>> simplify_fraction(\"test\",\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: The arguments must been from type int and 'denominator' != 0"
  [numerator denominator] (let [gcd-of-fraction (atom nil)] (try (assert (py-truthy (let [__b2 (integer? numerator)] (if (py-truthy __b2) (let [__b1 (integer? denominator)] (if (py-truthy __b1) (not (py-eq denominator 0)) __b1)) __b2))) (py-str "The arguments must been from type int and 'denominator' != 0")) (reset! gcd-of-fraction (m1/gcd-by-iterative (py-abs numerator) (py-abs denominator))) (assert (py-truthy (let [__b4 (integer? @gcd-of-fraction)] (if (py-truthy __b4) (let [__b3 (py-eq (py-mod numerator @gcd-of-fraction) 0)] (if (py-truthy __b3) (py-eq (py-mod denominator @gcd-of-fraction) 0) __b3)) __b4))) (py-str "Error in function gcd_by_iterative(...,...)")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-floordiv numerator @gcd-of-fraction) (py-floordiv denominator @gcd-of-fraction)]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn factorial "input: positive integer 'n'\nreturns the factorial of 'n' (n!)\n\n>>> factorial(0)\n1\n>>> factorial(20)\n2432902008176640000\n>>> factorial(-1)\nTraceback (most recent call last):\n    ...\nAssertionError: 'n' must been a int and >= 0\n>>> factorial(\"test\")\nTraceback (most recent call last):\n    ...\nAssertionError: 'n' must been a int and >= 0"
  [n] (let [ans (atom nil) factor (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-ge n 0) __b1))) (py-str "'n' must been a int and >= 0")) (reset! ans 1) (doseq [__i2 (py-iter (range 1 (py-add n 1)))] (reset! factor __i2) (swap! ans py-mul @factor)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn fib "input: positive integer 'n'\nreturns the n-th fibonacci term , indexing by 0\n\n>>> fib(0)\n1\n>>> fib(5)\n8\n>>> fib(20)\n10946\n>>> fib(99)\n354224848179261915075\n>>> fib(-1)\nTraceback (most recent call last):\n...\nAssertionError: 'n' must been an int and >= 0\n>>> fib(\"test\")\nTraceback (most recent call last):\n...\nAssertionError: 'n' must been an int and >= 0"
  [n] (let [--v (atom nil) ans (atom nil) fib1 (atom nil) tmp (atom nil)] (try (assert (py-truthy (let [__b1 (integer? n)] (if (py-truthy __b1) (py-ge n 0) __b1))) (py-str "'n' must been an int and >= 0")) (reset! tmp 0) (reset! fib1 1) (reset! ans 1) (doseq [__i2 (py-iter (range (py-sub n 1)))] (reset! --v __i2) (reset! tmp @ans) (swap! ans py-add @fib1) (reset! fib1 @tmp)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
