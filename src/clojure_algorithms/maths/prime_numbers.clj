(ns clojure-algorithms.maths.prime-numbers
  "Port of maths/prime_numbers.py"
  (:require [clojure.string :as str]))

(declare slow-primes primes fast-primes benchmark)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-mod py-int)

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

(defn- py-int [x base]
  (cond (boolean? x) (if x 1 0)
        (char? x) (py-int (str x) base)
        (integer? x) (if (nil? base) x
                         (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (ratio? x) (if (nil? base) (long x)
                       (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (decimal? x) (if (nil? base) (long x)
                         (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (float? x) (if (nil? base)
                     (try (long x)
                          (catch Exception e
                            (throw (ex-info "cannot convert float to int" {:type :OverflowError}))))
                     (throw (ex-info "int() can't convert non-string with explicit base" {:type :TypeError})))
        (string? x)
        (let [s (clojure.string/replace (clojure.string/trim x) "_" "")]
          (try
            (if (nil? base)
              (bigint (java.math.BigInteger. s 10))
              (let [b (long base)]
                (cond (= b 0) (cond (re-matches #"(?i)[+-]?0x[0-9a-f]+" s) (bigint (java.math.BigInteger. s 16))
                                    (re-matches #"(?i)[+-]?0o[0-7]+" s) (bigint (java.math.BigInteger. (clojure.string/replace s #"(?i)0o" "") 8))
                                    (re-matches #"(?i)[+-]?0b[01]+" s) (bigint (java.math.BigInteger. (clojure.string/replace s #"(?i)0b" "") 2))
                                    :else (bigint (java.math.BigInteger. s 10)))
                      (or (< b 2) (> b 36)) (throw (ex-info "int() base must be >= 2 and <= 36" {:type :ValueError}))
                      :else (bigint (java.math.BigInteger. s (int b))))))
            (catch clojure.lang.ExceptionInfo e (throw e))
            (catch NumberFormatException e
              (throw (ex-info (str "invalid literal for int(): " x) {:type :ValueError})))))
        :else (throw (ex-info (str "int() argument must be a string or a number") {:type :TypeError}))))

(defn slow-primes "Return a list of all primes numbers up to max.\n>>> list(slow_primes(0))\n[]\n>>> list(slow_primes(-1))\n[]\n>>> list(slow_primes(-10))\n[]\n>>> list(slow_primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(slow_primes(11))\n[2, 3, 5, 7, 11]\n>>> list(slow_primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(slow_primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1)))] i))) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 2 @i))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn primes "Return a list of all primes numbers up to max.\n>>> list(primes(0))\n[]\n>>> list(primes(-1))\n[]\n>>> list(primes(-10))\n[]\n>>> list(primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(primes(11))\n[2, 3, 5, 7, 11]\n>>> list(primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) bound (atom nil) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1)))] i))) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (reset! bound (py-add (py-int (Math/sqrt @i) nil) 1)) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 2 @bound))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn fast-primes "Return a list of all primes numbers up to max.\n>>> list(fast_primes(0))\n[]\n>>> list(fast_primes(-1))\n[]\n>>> list(fast_primes(-10))\n[]\n>>> list(fast_primes(25))\n[2, 3, 5, 7, 11, 13, 17, 19, 23]\n>>> list(fast_primes(11))\n[2, 3, 5, 7, 11]\n>>> list(fast_primes(33))\n[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]\n>>> list(fast_primes(1000))[-1]\n997"
  [max-n] (let [__ys (atom []) bound (atom nil) i (atom nil) j (atom nil) numbers (atom nil)] (try (reset! numbers (doall (for [i (py-iter (range 1 (py-add max-n 1) 2))] i))) (when (py-truthy (py-gt max-n 2)) (do (swap! __ys conj 2) nil)) (doseq [__i1 (py-iter (doall (for [n (py-iter @numbers) :when (py-truthy (py-gt n 1))] n)))] (reset! i __i1) (reset! bound (py-add (py-int (Math/sqrt @i) nil) 1)) (let [__nb3 (atom true)] (try (doseq [__i2 (py-iter (range 3 @bound 2))] (reset! j __i2) (when (py-truthy (py-eq (py-mod @i @j) 0)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) (reset! __nb3 false) (throw __e)))) (when @__nb3 (do (swap! __ys conj @i) nil)))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn benchmark "Let's benchmark our functions side-by-side..."
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
