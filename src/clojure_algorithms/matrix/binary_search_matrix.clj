(ns clojure-algorithms.matrix.binary-search-matrix
  "Port of matrix/binary_search_matrix.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare binary-search mat-bin-search)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-index-norm py-get py-add py-sub py-floordiv py-neg py-float-str py-quote py-repr py-int)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

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

(defn binary-search "This function carries out Binary search on a 1d array and\nreturn -1 if it do not exist\narray: A 1d sorted array\nvalue : the value meant to be searched\n>>> matrix = [1, 4, 7, 11, 15]\n>>> binary_search(matrix, 0, len(matrix) - 1, 1)\n0\n>>> binary_search(matrix, 0, len(matrix) - 1, 23)\n-1"
  [array lower-bound upper-bound value] (let [r (atom nil)] (try (reset! r (py-int (py-floordiv (py-add lower-bound upper-bound) 2) nil)) (when (py-truthy (py-eq (py-get array @r) value)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @r}))) (when (py-truthy (py-ge lower-bound upper-bound)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-neg 1)}))) (if (py-truthy (py-lt (py-get array @r) value)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (binary-search array (py-add @r 1) upper-bound value)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (binary-search array lower-bound (py-sub @r 1) value)})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn mat-bin-search "This function loops over a 2d matrix and calls binarySearch on\nthe selected 1d array and returns [-1, -1] is it do not exist\nvalue : value meant to be searched\nmatrix = a sorted 2d matrix\n>>> matrix = [[1, 4, 7, 11, 15],\n...           [2, 5, 8, 12, 19],\n...           [3, 6, 9, 16, 22],\n...           [10, 13, 14, 17, 24],\n...           [18, 21, 23, 26, 30]]\n>>> target = 1\n>>> mat_bin_search(target, matrix)\n[0, 0]\n>>> target = 34\n>>> mat_bin_search(target, matrix)\n[-1, -1]"
  [value matrix] (let [index (atom nil) r (atom nil)] (try (reset! index 0) (when (py-truthy (py-eq (py-get (py-get matrix @index) 0) value)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@index 0]}))) (while (py-truthy (let [__b1 (py-lt @index (count matrix))] (if (py-truthy __b1) (py-lt (py-get (py-get matrix @index) 0) value) __b1))) (reset! r (binary-search (py-get matrix @index) 0 (py-sub (count (py-get matrix @index)) 1) value)) (when (py-truthy (not (py-eq @r (py-neg 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@index @r]}))) (swap! index py-add 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-neg 1) (py-neg 1)]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
