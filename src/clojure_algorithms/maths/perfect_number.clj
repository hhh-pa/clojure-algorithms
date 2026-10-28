(ns clojure-algorithms.maths.perfect-number
  "== Perfect Number ==\nIn number theory, a perfect number is a positive integer that is equal to the sum of\nits positive divisors, excluding the number itself.\n\nFor example: 6 ==> divisors[1, 2, 3, 6]\n    Excluding 6, the sum(divisors) is 1 + 2 + 3 = 6\n    So, 6 is a Perfect Number\n\nThe first few perfect numbers are: 6, 28, 496, 8128, 33550336, ...\n\nhttps://en.wikipedia.org/wiki/Perfect_number\nhttps://oeis.org/A000396"
  (:require [clojure.string :as str]))

(declare perfect perfect-optimized find-perfect-numbers get-divisors)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-repeat-vec py-mul py-floordiv py-mod py-float-str py-quote py-repr py-str)

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

(defn perfect "Check if a number is a perfect number.\n\nA perfect number is a positive integer that is equal to the sum of its proper\ndivisors (positive divisors excluding the number itself).\n\nThe algorithm finds all divisors up to number//2 (since no proper divisor\ncan be greater than half the number) and sums them for comparison.\n\nTime Complexity: O(sqrt(n)) with optimized divisor finding\nSpace Complexity: O(1)\n\nArgs:\n    number: The positive integer to be checked.\n\nReturns:\n    True if the number is a perfect number, False otherwise.\n\nRaises:\n    ValueError: If number is not an integer.\n\nExamples:\n    Basic perfect numbers:\n    >>> perfect(6)\n    True\n    >>> perfect(28)\n    True\n    >>> perfect(496)\n    True\n    >>> perfect(8128)\n    True\n\n    Large perfect number:\n    >>> perfect(33550336)\n    True\n\n    Non-perfect numbers:\n    >>> perfect(12)\n    False\n    >>> perfect(27)\n    False\n    >>> perfect(29)\n    False\n    >>> perfect(100)\n    False\n\n    Edge cases:\n    >>> perfect(1)\n    False\n    >>> perfect(2)\n    False\n    >>> perfect(0)\n    False\n    >>> perfect(-1)\n    False\n    >>> perfect(-6)\n    False\n\n    Numbers close to perfect numbers:\n    >>> perfect(5)\n    False\n    >>> perfect(7)\n    False\n    >>> perfect(27)\n    False\n    >>> perfect(29)\n    False\n    >>> perfect(495)\n    False\n    >>> perfect(497)\n    False\n    >>> perfect(33550335)\n    False\n    >>> perfect(33550337)\n    False\n\n    Type validation:\n    >>> perfect(12.34)\n    Traceback (most recent call last):\n    ...\n    ValueError: number must be an integer\n    >>> perfect(\"123\")\n    Traceback (most recent call last):\n    ...\n    ValueError: number must be an integer\n    >>> perfect(\"Hello\")\n    Traceback (most recent call last):\n    ...\n    ValueError: number must be an integer\n    >>> perfect([6])\n    Traceback (most recent call last):\n    ...\n    ValueError: number must be an integer\n    >>> perfect(None)\n    Traceback (most recent call last):\n    ...\n    ValueError: number must be an integer\n\n    Testing divisor sum calculation for known cases:\n    >>> # For 6: divisors are 1, 2, 3 -> sum = 6\n    >>> sum(i for i in range(1, 6//2 + 1) if 6 % i == 0) == 6\n    True\n    >>> # For 28: divisors are 1, 2, 4, 7, 14 -> sum = 28\n    >>> sum(i for i in range(1, 28//2 + 1) if 28 % i == 0) == 28\n    True\n    >>> # For 12: divisors are 1, 2, 3, 4, 6 -> sum = 16 ≠ 12\n    >>> sum(i for i in range(1, 12//2 + 1) if 12 % i == 0) == 12\n    False"
  [number] (let [divisor-sum (atom nil)] (try (when (py-truthy (not (py-truthy (integer? number)))) (throw (ex-info (py-str "number must be an integer") {:type :ValueError}))) (when (py-truthy (py-le number 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (py-eq number 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (reset! divisor-sum (reduce +' 0 (doall (for [i (py-iter (range 1 (py-add (py-floordiv number 2) 1))) :when (py-truthy (py-eq (py-mod number i) 0))] i)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @divisor-sum number)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn perfect-optimized "Optimized version of perfect number checker using mathematical properties.\n\nThis version uses the fact that divisors come in pairs (d, n/d) to reduce\nthe search space to sqrt(n).\n\nTime Complexity: O(sqrt(n))\nSpace Complexity: O(1)\n\nArgs:\n    number: The positive integer to be checked.\n\nReturns:\n    True if the number is a perfect number, False otherwise.\n\nExamples:\n    >>> perfect_optimized(6)\n    True\n    >>> perfect_optimized(28)\n    True\n    >>> perfect_optimized(496)\n    True\n    >>> perfect_optimized(12)\n    False\n    >>> perfect_optimized(1)\n    False\n    >>> perfect_optimized(0)\n    False\n    >>> perfect_optimized(-1)\n    False"
  [number] (let [divisor-sum (atom nil) i (atom nil)] (try (when (py-truthy (not (py-truthy (integer? number)))) (throw (ex-info (py-str "number must be an integer") {:type :ValueError}))) (when (py-truthy (py-le number 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (reset! divisor-sum 1) (reset! i 2) (while (py-truthy (py-le (py-mul @i @i) number)) (when (py-truthy (py-eq (py-mod number @i) 0)) (swap! divisor-sum py-add @i) (when (py-truthy (not (py-eq @i (py-floordiv number @i)))) (swap! divisor-sum py-add (py-floordiv number @i)))) (swap! i py-add 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @divisor-sum number)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn find-perfect-numbers "Find all perfect numbers up to a given limit.\n\nArgs:\n    limit: The upper bound to search for perfect numbers.\n\nReturns:\n    List of perfect numbers up to the limit.\n\nExamples:\n    >>> find_perfect_numbers(10)\n    [6]\n    >>> find_perfect_numbers(30)\n    [6, 28]\n    >>> find_perfect_numbers(500)\n    [6, 28, 496]\n    >>> find_perfect_numbers(0)\n    []\n    >>> find_perfect_numbers(1)\n    []"
  [limit] (try (when (py-truthy (let [__b1 (not (py-truthy (integer? limit)))] (if (py-truthy __b1) __b1 (py-lt limit 0)))) (throw (ex-info (py-str "limit must be a non-negative integer") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [n (py-iter (range 1 (py-add limit 1))) :when (py-truthy (perfect n))] n))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn get-divisors "Get all proper divisors of a number (excluding the number itself).\n\nArgs:\n    number: The positive integer to find divisors for.\n\nReturns:\n    List of proper divisors in ascending order.\n\nExamples:\n    >>> get_divisors(6)\n    [1, 2, 3]\n    >>> get_divisors(28)\n    [1, 2, 4, 7, 14]\n    >>> get_divisors(12)\n    [1, 2, 3, 4, 6]\n    >>> get_divisors(1)\n    []\n    >>> get_divisors(7)\n    [1]"
  [number] (try (when (py-truthy (let [__b1 (not (py-truthy (integer? number)))] (if (py-truthy __b1) __b1 (py-le number 0)))) (throw (ex-info (py-str "number must be a positive integer") {:type :ValueError}))) (when (py-truthy (py-eq number 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [i (py-iter (range 1 (py-add (py-floordiv number 2) 1))) :when (py-truthy (py-eq (py-mod number i) 0))] i))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
