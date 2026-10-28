(ns clojure-algorithms.maths.binary-exponentiation
  "Binary Exponentiation\n\nThis is a method to find a^b in O(log b) time complexity and is one of the most commonly\nused methods of exponentiation. The method is also useful for modular exponentiation,\nwhen the solution to (a^b) % c is required.\n\nTo calculate a^b:\n- If b is even, then a^b = (a * a)^(b / 2)\n- If b is odd, then a^b = a * a^(b - 1)\nRepeat until b = 1 or b = 0\n\nFor modular exponentiation, we use the fact that (a * b) % c = ((a % c) * (b % c)) % c"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare binary-exp-recursive binary-exp-iterative binary-exp-mod-recursive binary-exp-mod-iterative)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-sub py-repeat-vec py-mul py-floordiv py-mod py-band py-float-str py-quote py-repr py-str)

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

(defn binary-exp-recursive "Computes a^b recursively, where a is the base and b is the exponent\n\n>>> binary_exp_recursive(3, 5)\n243\n>>> binary_exp_recursive(11, 13)\n34522712143931\n>>> binary_exp_recursive(-1, 3)\n-1\n>>> binary_exp_recursive(0, 5)\n0\n>>> binary_exp_recursive(3, 1)\n3\n>>> binary_exp_recursive(3, 0)\n1\n>>> binary_exp_recursive(1.5, 4)\n5.0625\n>>> binary_exp_recursive(3, -1)\nTraceback (most recent call last):\n    ...\nValueError: Exponent must be a non-negative integer"
  [base exponent] (let [b (atom nil)] (try (when (py-truthy (py-lt exponent 0)) (throw (ex-info (py-str "Exponent must be a non-negative integer") {:type :ValueError}))) (when (py-truthy (py-eq exponent 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (when (py-truthy (py-eq (py-mod exponent 2) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (binary-exp-recursive base (py-sub exponent 1)) base)}))) (reset! b (binary-exp-recursive base (py-floordiv exponent 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul @b @b)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn binary-exp-iterative "Computes a^b iteratively, where a is the base and b is the exponent\n\n>>> binary_exp_iterative(3, 5)\n243\n>>> binary_exp_iterative(11, 13)\n34522712143931\n>>> binary_exp_iterative(-1, 3)\n-1\n>>> binary_exp_iterative(0, 5)\n0\n>>> binary_exp_iterative(3, 1)\n3\n>>> binary_exp_iterative(3, 0)\n1\n>>> binary_exp_iterative(1.5, 4)\n5.0625\n>>> binary_exp_iterative(3, -1)\nTraceback (most recent call last):\n    ...\nValueError: Exponent must be a non-negative integer"
  [base exponent] (let [base (atom base) exponent (atom exponent)] (let [res (atom nil)] (try (when (py-truthy (py-lt @exponent 0)) (throw (ex-info (py-str "Exponent must be a non-negative integer") {:type :ValueError}))) (reset! res 1) (while (py-truthy (py-gt @exponent 0)) (when (py-truthy (py-band @exponent 1)) (swap! res py-mul @base)) (swap! base py-mul @base) (swap! exponent (fn [__o] (.shiftRight (.toBigInteger (bigint __o)) (int 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn binary-exp-mod-recursive "Computes a^b % c recursively, where a is the base, b is the exponent, and c is the\nmodulus\n\n>>> binary_exp_mod_recursive(3, 4, 5)\n1\n>>> binary_exp_mod_recursive(11, 13, 7)\n4\n>>> binary_exp_mod_recursive(1.5, 4, 3)\n2.0625\n>>> binary_exp_mod_recursive(7, -1, 10)\nTraceback (most recent call last):\n    ...\nValueError: Exponent must be a non-negative integer\n>>> binary_exp_mod_recursive(7, 13, 0)\nTraceback (most recent call last):\n    ...\nValueError: Modulus must be a positive integer"
  [base exponent modulus] (let [r (atom nil)] (try (when (py-truthy (py-lt exponent 0)) (throw (ex-info (py-str "Exponent must be a non-negative integer") {:type :ValueError}))) (when (py-truthy (py-le modulus 0)) (throw (ex-info (py-str "Modulus must be a positive integer") {:type :ValueError}))) (when (py-truthy (py-eq exponent 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (when (py-truthy (py-eq (py-mod exponent 2) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-mul (binary-exp-mod-recursive base (py-sub exponent 1) modulus) base) modulus)}))) (reset! r (binary-exp-mod-recursive base (py-floordiv exponent 2) modulus)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-mul @r @r) modulus)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn binary-exp-mod-iterative "Computes a^b % c iteratively, where a is the base, b is the exponent, and c is the\nmodulus\n\n>>> binary_exp_mod_iterative(3, 4, 5)\n1\n>>> binary_exp_mod_iterative(11, 13, 7)\n4\n>>> binary_exp_mod_iterative(1.5, 4, 3)\n2.0625\n>>> binary_exp_mod_iterative(7, -1, 10)\nTraceback (most recent call last):\n    ...\nValueError: Exponent must be a non-negative integer\n>>> binary_exp_mod_iterative(7, 13, 0)\nTraceback (most recent call last):\n    ...\nValueError: Modulus must be a positive integer"
  [base exponent modulus] (let [base (atom base) exponent (atom exponent)] (let [res (atom nil)] (try (when (py-truthy (py-lt @exponent 0)) (throw (ex-info (py-str "Exponent must be a non-negative integer") {:type :ValueError}))) (when (py-truthy (py-le modulus 0)) (throw (ex-info (py-str "Modulus must be a positive integer") {:type :ValueError}))) (reset! res 1) (while (py-truthy (py-gt @exponent 0)) (when (py-truthy (py-band @exponent 1)) (reset! res (py-mod (py-mul (py-mod @res modulus) (py-mod @base modulus)) modulus))) (swap! base py-mul @base) (swap! exponent (fn [__o] (.shiftRight (.toBigInteger (bigint __o)) (int 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @res})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
