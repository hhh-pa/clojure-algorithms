(ns clojure-algorithms.maths.keith-number
  "== Keith Number (Repfigit Number) ==\nA Keith number (also known as a repfigit number, short for \"repetitive\nFibonacci-like digit\") is a natural number n with d >= 2 decimal digits\nsuch that when a sequence is formed starting with the d digits of n and each\nsubsequent term is the sum of the preceding d terms, the number n itself\nappears in the sequence.\n\nFor example, 197 is a 3-digit number (d = 3):\n    Initial terms: 1, 9, 7\n    Next term: 1 + 9 + 7 = 17\n    Next term: 9 + 7 + 17 = 33\n    Next term: 7 + 17 + 33 = 57\n    Next term: 17 + 33 + 57 = 107\n    Next term: 33 + 57 + 107 = 197 (Matches 197! So 197 is a Keith number).\n\nThe first few Keith numbers are:\n14, 19, 28, 47, 61, 75, 197, 742, 1104, 1537, 2208, 2580, 3684, 4788, 7385...\n\nReferences:\n- https://en.wikipedia.org/wiki/Keith_number\n- https://oeis.org/A007629"
  (:require [clojure.string :as str]))

(declare is-keith-number find-keith-numbers)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-float-str py-quote py-repr py-str py-int)

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

(defn is-keith-number "Check if a given integer is a Keith number.\n\nA Keith number is an integer greater than or equal to 10 that appears\nin the Fibonacci-like sequence generated by its digits. Single-digit\nnumbers are excluded by mathematical convention.\n\nComplexity Analysis:\n    - Time Complexity: O(d * k) where d is the number of digits of number\n      and k is the number of terms until the sequence reaches or exceeds\n      the number. Since the terms grow exponentially, k = O(log(number)).\n    - Space Complexity: O(d) auxiliary space to store the previous d terms.\n\nArgs:\n    number: The integer to test.\n\nReturns:\n    True if number is a Keith number, False otherwise.\n\nRaises:\n    TypeError: If number is not an integer.\n\nExamples:\n    Known 2-digit Keith numbers:\n    >>> is_keith_number(14)\n    True\n    >>> is_keith_number(19)\n    True\n    >>> is_keith_number(28)\n    True\n    >>> is_keith_number(47)\n    True\n    >>> is_keith_number(61)\n    True\n    >>> is_keith_number(75)\n    True\n\n    Known 3-digit and 4-digit Keith numbers:\n    >>> is_keith_number(197)\n    True\n    >>> is_keith_number(742)\n    True\n    >>> is_keith_number(1104)\n    True\n    >>> is_keith_number(1537)\n    True\n\n    Non-Keith numbers:\n    >>> is_keith_number(10)\n    False\n    >>> is_keith_number(12)\n    False\n    >>> is_keith_number(25)\n    False\n    >>> is_keith_number(100)\n    False\n\n    Edge cases (single-digit and non-positive integers):\n    >>> is_keith_number(9)\n    False\n    >>> is_keith_number(1)\n    False\n    >>> is_keith_number(0)\n    False\n    >>> is_keith_number(-14)\n    False\n\n    Type validation:\n    >>> is_keith_number(14.0)\n    Traceback (most recent call last):\n        ...\n    TypeError: number must be an integer\n    >>> is_keith_number(\"14\")\n    Traceback (most recent call last):\n        ...\n    TypeError: number must be an integer\n    >>> is_keith_number(True)\n    Traceback (most recent call last):\n        ...\n    TypeError: number must be an integer\n    >>> is_keith_number(None)\n    Traceback (most recent call last):\n        ...\n    TypeError: number must be an integer"
  [& _args]
  ;; stub: original requires method .popleft; pure-Clojure port not applicable
  (throw (ex-info "requires method .popleft" {:type :UnsupportedOperation})))

(defn find-keith-numbers "Find and return all Keith numbers up to a specified limit.\n\nComplexity Analysis:\n    - Time Complexity: O(limit * log(limit))\n    - Space Complexity: O(k) where k is the number of Keith numbers found.\n\nArgs:\n    limit: The upper bound (inclusive) up to which to search for Keith numbers.\n\nReturns:\n    A list of Keith numbers less than or equal to limit.\n\nRaises:\n    TypeError: If limit is not an integer.\n    ValueError: If limit is less than 10.\n\nExamples:\n    >>> find_keith_numbers(100)\n    [14, 19, 28, 47, 61, 75]\n    >>> find_keith_numbers(200)\n    [14, 19, 28, 47, 61, 75, 197]\n    >>> find_keith_numbers(10)\n    []\n\n    Type and value errors:\n    >>> find_keith_numbers(9)\n    Traceback (most recent call last):\n        ...\n    ValueError: limit must be an integer greater than or equal to 10\n    >>> find_keith_numbers(10.5)\n    Traceback (most recent call last):\n        ...\n    TypeError: limit must be an integer\n    >>> find_keith_numbers(False)\n    Traceback (most recent call last):\n        ...\n    TypeError: limit must be an integer"
  [limit] (try (when (py-truthy (let [__b1 (not (py-truthy (integer? limit)))] (if (py-truthy __b1) __b1 (boolean? limit)))) (throw (ex-info (py-str "limit must be an integer") {:type :TypeError}))) (when (py-truthy (py-lt limit 10)) (throw (ex-info (py-str "limit must be an integer greater than or equal to 10") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [num (py-iter (range 10 (py-add limit 1))) :when (py-truthy (is-keith-number num))] num))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

;; NOTE: is_keith_number stubbed (method .popleft)
