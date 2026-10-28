(ns clojure-algorithms.maths.special-numbers.harshad-numbers
  "A harshad number (or more specifically an n-harshad number) is a number that's\ndivisible by the sum of its digits in some given base n.\nReference: https://en.wikipedia.org/wiki/Harshad_number"
  (:require [clojure.string :as str]))

(declare int-to-base sum-of-digits harshad-numbers-in-base is-harshad-number-in-base)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-floordiv py-mod py-float-str py-quote py-repr py-str py-int)

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

(defn int-to-base "Convert a given positive decimal integer to base 'base'.\nWhere 'base' ranges from 2 to 36.\n\nExamples:\n>>> int_to_base(0, 21)\n'0'\n>>> int_to_base(23, 2)\n'10111'\n>>> int_to_base(58, 5)\n'213'\n>>> int_to_base(167, 16)\n'A7'\n>>> # bases below 2 and beyond 36 will error\n>>> int_to_base(98, 1)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> int_to_base(98, 37)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> int_to_base(-99, 16)\nTraceback (most recent call last):\n    ...\nValueError: number must be a positive integer"
  [number base] (let [number (atom number)] (let [digits (atom nil) remainder (atom nil) result (atom nil)] (try (when (py-truthy (let [__b1 (py-lt base 2)] (if (py-truthy __b1) __b1 (py-gt base 36)))) (throw (ex-info (py-str "'base' must be between 2 and 36 inclusive") {:type :ValueError}))) (reset! digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ") (reset! result "") (when (py-truthy (py-lt @number 0)) (throw (ex-info (py-str "number must be a positive integer") {:type :ValueError}))) (while (py-truthy (py-gt @number 0)) (let [__d2 (vec [(py-floordiv @number base) (py-mod @number base)])] (reset! number (nth __d2 0)) (reset! remainder (nth __d2 1))) (reset! result (py-add (py-get @digits @remainder) @result))) (when (py-truthy (py-eq @result "")) (reset! result "0")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn sum-of-digits "Calculate the sum of digit values in a positive integer\nconverted to the given 'base'.\nWhere 'base' ranges from 2 to 36.\n\nExamples:\n>>> sum_of_digits(103, 12)\n'13'\n>>> sum_of_digits(1275, 4)\n'30'\n>>> sum_of_digits(6645, 2)\n'1001'\n>>> # bases below 2 and beyond 36 will error\n>>> sum_of_digits(543, 1)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> sum_of_digits(543, 37)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive"
  [num base] (let [num-str (atom nil) res (atom nil) res-str (atom nil)] (try (when (py-truthy (let [__b1 (py-lt base 2)] (if (py-truthy __b1) __b1 (py-gt base 36)))) (throw (ex-info (py-str "'base' must be between 2 and 36 inclusive") {:type :ValueError}))) (reset! num-str (int-to-base num base)) (reset! res (reduce +' 0 (doall (for [char (py-iter @num-str)] (py-int char base))))) (reset! res-str (int-to-base @res base)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res-str})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn harshad-numbers-in-base "Finds all Harshad numbers smaller than num in base 'base'.\nWhere 'base' ranges from 2 to 36.\n\nExamples:\n>>> harshad_numbers_in_base(15, 2)\n['1', '10', '100', '110', '1000', '1010', '1100']\n>>> harshad_numbers_in_base(12, 34)\n['1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B']\n>>> harshad_numbers_in_base(12, 4)\n['1', '2', '3', '10', '12', '20', '21']\n>>> # bases below 2 and beyond 36 will error\n>>> harshad_numbers_in_base(234, 37)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> harshad_numbers_in_base(234, 1)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> harshad_numbers_in_base(-12, 6)\n[]"
  [limit base] (let [numbers (atom nil)] (try (when (py-truthy (let [__b1 (py-lt base 2)] (if (py-truthy __b1) __b1 (py-gt base 36)))) (throw (ex-info (py-str "'base' must be between 2 and 36 inclusive") {:type :ValueError}))) (when (py-truthy (py-lt limit 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (reset! numbers (vec (for [i (py-iter (range 1 limit)) :when (py-truthy (py-eq (py-mod i (py-int (sum-of-digits i base) base)) 0))] (int-to-base i base)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @numbers})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn is-harshad-number-in-base "Determines whether n in base 'base' is a harshad number.\nWhere 'base' ranges from 2 to 36.\n\nExamples:\n>>> is_harshad_number_in_base(18, 10)\nTrue\n>>> is_harshad_number_in_base(21, 10)\nTrue\n>>> is_harshad_number_in_base(-21, 5)\nFalse\n>>> # bases below 2 and beyond 36 will error\n>>> is_harshad_number_in_base(45, 37)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive\n>>> is_harshad_number_in_base(45, 1)\nTraceback (most recent call last):\n    ...\nValueError: 'base' must be between 2 and 36 inclusive"
  [num base] (let [d (atom nil) n (atom nil)] (try (when (py-truthy (let [__b1 (py-lt base 2)] (if (py-truthy __b1) __b1 (py-gt base 36)))) (throw (ex-info (py-str "'base' must be between 2 and 36 inclusive") {:type :ValueError}))) (when (py-truthy (py-lt num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (reset! n (int-to-base num base)) (reset! d (sum-of-digits num base)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (py-mod (py-int @n base) (py-int @d base)) 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
