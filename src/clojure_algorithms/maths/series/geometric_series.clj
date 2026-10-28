(ns clojure-algorithms.maths.series.geometric-series
  "This is a pure Python implementation of the Geometric Series algorithm\nhttps://en.wikipedia.org/wiki/Geometric_series\nRun the doctests with the following command:\npython3 -m doctest -v geometric_series.py\nor\npython -m doctest -v geometric_series.py\nFor manual testing run:\npython3 geometric_series.py"
  (:require [clojure.string :as str]))

(declare geometric-series)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-add py-repeat-vec py-mul py-pow py-powmod py-int py-float)

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

(defn- py-powmod [a b m]
  (let [a (py-num-bool a) b (py-num-bool b) m (py-num-bool m)]
    (when (or (not (integer? a)) (not (integer? b)) (not (integer? m)))
      (throw (ex-info "pow() 3rd argument not allowed unless all arguments are integers" {:type :TypeError})))
    (when (zero? m) (throw (ex-info "pow() 3rd argument cannot be 0" {:type :ValueError})))
    (let [bi (biginteger a) e (long b) mm (biginteger m)]
      (bigint (if (neg? b)
                (.modPow (.modInverse bi mm) (java.math.BigInteger/valueOf (- e)) mm)
                (.modPow bi (java.math.BigInteger/valueOf e) mm))))))

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

(defn- py-float [x]
  (cond (boolean? x) (if x 1.0 0.0)
        (integer? x) (double x)
        (float? x) (double x)
        (string? x)
        (let [s (-> x clojure.string/trim (clojure.string/replace "_" ""))
              l (clojure.string/lower-case s)]
          (cond (contains? #{"inf" "+inf" "infinity" "+infinity"} l) Double/POSITIVE_INFINITY
                (contains? #{"-inf" "-infinity"} l) Double/NEGATIVE_INFINITY
                (contains? #{"nan" "+nan" "-nan"} l) Double/NaN
                :else (try (Double/parseDouble s)
                           (catch NumberFormatException e
                             (throw (ex-info (str "could not convert string to float: " x) {:type :ValueError}))))))
        :else (throw (ex-info "float() argument must be a string or a number" {:type :TypeError}))))

(defn geometric-series "Pure Python implementation of Geometric Series algorithm\n\n:param nth_term: The last term (nth term of Geometric Series)\n:param start_term_a : The first term of Geometric Series\n:param common_ratio_r : The common ratio between all the terms\n:return: The Geometric Series starting from first term a and multiple of common\n    ration with first term with increase in power till last term (nth term)\nExamples:\n>>> geometric_series(4, 2, 2)\n[2, 4.0, 8.0, 16.0]\n>>> geometric_series(4.0, 2.0, 2.0)\n[2.0, 4.0, 8.0, 16.0]\n>>> geometric_series(4.1, 2.1, 2.1)\n[2.1, 4.41, 9.261000000000001, 19.448100000000004]\n>>> geometric_series(4, 2, -2)\n[2, -4.0, 8.0, -16.0]\n>>> geometric_series(4, -2, 2)\n[-2, -4.0, -8.0, -16.0]\n>>> geometric_series(-4, 2, 2)\n[]\n>>> geometric_series(0, 100, 500)\n[]\n>>> geometric_series(1, 1, 1)\n[1]\n>>> geometric_series(0, 0, 0)\n[]"
  [nth-term start-term-a common-ratio-r] (let [--v (atom nil) multiple (atom nil) power (atom nil) series (atom nil)] (try (when (py-truthy (not (py-truthy (every? py-truthy [nth-term start-term-a common-ratio-r])))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (reset! series []) (reset! power 1) (reset! multiple common-ratio-r) (doseq [__i1 (py-iter (range (py-int nth-term nil)))] (reset! --v __i1) (if (py-truthy (not (py-truthy @series))) (do (swap! series conj start-term-a)) (do (swap! power py-add 1) (swap! series conj (py-float (py-mul start-term-a @multiple))) (reset! multiple (py-pow (py-float common-ratio-r) @power))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @series})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
