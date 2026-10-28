(ns clojure-algorithms.maths.special-numbers.kaprekar-constant
  "Kaprekar's Constant (6174) - also known as Kaprekar's Routine\n\nKaprekar's constant is a special number discovered by Indian mathematician\nD. R. Kaprekar in 1949. It is notable for the following property:\n\n1. Take any four-digit number with at least two different digits (leading zeros allowed)\n2. Arrange the digits in descending order to form the largest possible number\n3. Arrange the digits in ascending order to form the smallest possible number\n4. Subtract the smaller number from the larger number\n5. Repeat the process with the result\n\nThis process will always reach 6174 in at most 7 iterations, and once 6174 is\nreached, the process will continue to yield 6174.\n\nExample:\n    3524 -> 5432 - 2345 = 3087\n    3087 -> 8730 - 0378 = 8352\n    8352 -> 8532 - 2358 = 6174\n    6174 -> 7641 - 1467 = 6174 (repeats)\n\nReference: https://en.wikipedia.org/wiki/6174_(number)\nOEIS: https://oeis.org/A099009"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare kaprekar-routine kaprekar-constant is-kaprekar-valid main)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-repeat-vec py-mul py-neg py-sorted py-set py-float-str py-quote py-repr py-str py-int py-zfill py-fmt-num py-fmt)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

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

(defn- py-zfill [s w]
  (let [s (str s) w (long w)]
    (if (>= (count s) w) s
        (if (and (seq s) (contains? #{\+ \-} (first s)))
          (str (first s) (apply str (repeat (- w (count s)) \0)) (subs s 1))
          (str (apply str (repeat (- w (count s)) \0)) s)))))

(defn- py-fmt-num [v conv prec comma?]
  (let [d (double v)
        s (case conv
            \f (format (str "%." prec "f") d)
            \F (clojure.string/upper-case (format (str "%." prec "f") d))
            \e (format (str "%." prec "e") d)
            \E (clojure.string/upper-case (format (str "%." prec "e") d))
            \g (let [p (max 1 prec)] (format (str "%." p "g") d))
            \G (let [p (max 1 prec)] (clojure.string/upper-case (format (str "%." p "g") d)))
            \d (if (integer? v) (str v) (str (long d)))
            \b (.toString (biginteger (long d)) 2)
            \o (.toString (biginteger (long d)) 8)
            \x (.toString (biginteger (long d)) 16)
            \X (clojure.string/upper-case (.toString (biginteger (long d)) 16))
            \c (str (char (long d)))
            \% (str (* d 100.0) "%")
            (str v))]
    (if comma? (clojure.string/replace s #"(\d)(?=(\d{3})+(?!\d))" "$1,") s)))

(defn- py-fmt [spec v]
  (if (or (nil? spec) (= spec ""))
    (py-str v)
    (let [spec (str spec)
          m (re-matches #"((.)?([<>=\^]))?([+\- ])?(#)?(0)?(\d+)?(,)?(?:\.(\d+))?([bcdeEfFgGnosxX%])?" spec)]
      (if (nil? m) (py-str v)
          (let [[_ _ fill align sign alt? zero? width comma? prec typ] m
                typ (if typ (first typ) nil)
                prec (if prec (Long/parseLong prec) (if (contains? #{\f \F \e \E} typ) 6 6))
                num? (or (number? v) (boolean? v))
                s (cond (nil? typ) (py-str v)
                        (= typ \s) (py-str v)
                        (= typ \c) (if (and (string? v) (= 1 (count v))) v
                                       (py-fmt-num (if (boolean? v) (if v 1 0) v) typ prec (some? comma?)))
                        (and (= typ \d) (not num?)) (throw (ex-info "format spec" {:type :ValueError}))
                        :else (py-fmt-num (if (boolean? v) (if v 1 0) v) typ prec (some? comma?)))
                s (if (and (= typ \s) prec (re-find #"\.\d+" spec))
                    (subs s 0 (min (count s) (Long/parseLong prec))) s)
                sign-str (if (and num? (number? v) (neg? (double v))) "" (case sign \+ "+" \  " " ""))
                s (if (and num? (contains? #{\d \f \F \e \E \g \G \%} typ) (not (neg? (double v)))) (str sign-str s) s)
                w (if width (Long/parseLong width) 0)
                pad (- w (count s))]
            (if (<= pad 0) s
                (let [al (or align (if num? \> \<))
                      f (cond fill fill (and zero? num? (= al \>)) \0 :else \space)]
                  (case al
                    \< (str s (apply str (repeat pad f)))
                    \> (str (apply str (repeat pad f)) s)
                    \^ (let [l (quot pad 2)] (str (apply str (repeat l f)) s (apply str (repeat (- pad l) f))))
                    \= (if (and (seq s) (contains? #{\+ \- \space} (first s)))
                         (str (first s) (apply str (repeat pad (if (= f \space) \0 f))) (subs s 1))
                         (str (apply str (repeat pad (if (= f \space) \0 f))) s))))))))))

(defn kaprekar-routine "Perform one iteration of Kaprekar's routine.\n\nArgs:\n    number: A 4-digit number (0-9999)\n\nReturns:\n    The result of one Kaprekar iteration\n\n>>> kaprekar_routine(3524)\n3087\n>>> kaprekar_routine(3087)\n8352\n>>> kaprekar_routine(8352)\n6174\n>>> kaprekar_routine(6174)\n6174\n>>> kaprekar_routine(1)\n999\n>>> kaprekar_routine(1111)\n0"
  [number] (let [ascending (atom nil) descending (atom nil) digits (atom nil) msg (atom nil)] (try (when (py-truthy (let [__b2 (not (py-truthy (integer? number)))] (if (py-truthy __b2) __b2 (let [__b1 (py-lt number 0)] (if (py-truthy __b1) __b1 (py-gt number 9999)))))) (reset! msg (str "Input must be a non-negative integer between 0 and 9999, got " (py-str number))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (reset! digits (py-zfill (py-str number) 4)) (when (py-truthy (py-eq (count (py-set @digits)) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! descending (py-int (clojure.string/join "" (map py-str (if (py-truthy true) (vec (reverse (py-sorted @digits))) (py-sorted @digits)))) nil)) (reset! ascending (py-int (clojure.string/join "" (map py-str (py-sorted @digits))) nil)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sub @descending @ascending)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn kaprekar-constant "Apply Kaprekar's routine until reaching the constant 6174 or max iterations.\n\nArgs:\n    number: A 4-digit number (0-9999)\n    max_iterations: Maximum number of iterations to perform (default: 7)\n\nReturns:\n    A tuple containing:\n    - The number of iterations taken to reach 6174 (or -1 if not reached)\n    - A list of all intermediate results\n\n>>> kaprekar_constant(3524)\n(3, [3524, 3087, 8352, 6174])\n>>> kaprekar_constant(6174)\n(0, [6174])\n>>> kaprekar_constant(1234)\n(3, [1234, 3087, 8352, 6174])\n>>> kaprekar_constant(1111)\n(-1, [1111, 0])\n>>> kaprekar_constant(495)\n(4, [495, 9081, 9621, 8352, 6174])\n>>> kaprekar_constant(9998)\n(5, [9998, 999, 8991, 8082, 8532, 6174])"
  ([number] (kaprekar-constant number 7))
  ([number max-iterations] (let [current (atom nil) iteration (atom nil) msg (atom nil) sequence (atom nil)] (try (when (py-truthy (let [__b2 (not (py-truthy (integer? number)))] (if (py-truthy __b2) __b2 (let [__b1 (py-lt number 0)] (if (py-truthy __b1) __b1 (py-gt number 9999)))))) (reset! msg (str "Input must be a non-negative integer between 0 and 9999, got " (py-str number))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (when (py-truthy (let [__b3 (not (py-truthy (integer? max-iterations)))] (if (py-truthy __b3) __b3 (py-lt max-iterations 1)))) (reset! msg (str "max_iterations must be a positive integer, got " (py-str max-iterations))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (reset! sequence [number]) (reset! current number) (when (py-truthy (py-eq @current 6174)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0 @sequence]}))) (doseq [__i4 (py-iter (range max-iterations))] (reset! iteration __i4) (reset! current (kaprekar-routine @current)) (swap! sequence conj @current) (when (py-truthy (py-eq @current 6174)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-add @iteration 1) @sequence]}))) (when (py-truthy (py-eq @current 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-neg 1) @sequence]})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [(py-neg 1) @sequence]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn is-kaprekar-valid "Check if a number is valid for Kaprekar's routine.\nA number is valid if it has at least two different digits.\n\nArgs:\n    number: A 4-digit number (0-9999)\n\nReturns:\n    True if the number is valid for Kaprekar's routine, False otherwise\n\n>>> is_kaprekar_valid(3524)\nTrue\n>>> is_kaprekar_valid(1111)\nFalse\n>>> is_kaprekar_valid(1000)\nTrue\n>>> is_kaprekar_valid(1)\nTrue\n>>> is_kaprekar_valid(6174)\nTrue"
  [number] (let [digits (atom nil) msg (atom nil)] (try (when (py-truthy (let [__b2 (not (py-truthy (integer? number)))] (if (py-truthy __b2) __b2 (let [__b1 (py-lt number 0)] (if (py-truthy __b1) __b1 (py-gt number 9999)))))) (reset! msg (str "Input must be a non-negative integer between 0 and 9999, got " (py-str number))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (reset! digits (py-zfill (py-str number) 4)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-gt (count (py-set @digits)) 1)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main "Demonstrate Kaprekar's constant with user input and examples."
  [] (let [examples (atom nil) iterations (atom nil) num (atom nil) sequence (atom nil) user-input (atom nil)] (println (clojure.string/join " " [(py-str "Kaprekar's Constant (6174) - Demonstration\n")])) (println (clojure.string/join " " [(py-str (py-mul "=" 50))])) (reset! examples [3524 1234 6174 9998 1111 5]) (doseq [__i1 (py-iter @examples)] (try (reset! num __i1) (println (clojure.string/join " " [(py-str (str "\nStarting number: " (py-fmt "04d" @num)))])) (when (py-truthy (not (py-truthy (is-kaprekar-valid @num)))) (println (clojure.string/join " " [(py-str "  ❌ Invalid: All digits are the same (repdigit)")])) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (let [__d2 (vec (kaprekar-constant @num))] (reset! iterations (nth __d2 0)) (reset! sequence (nth __d2 1))) (if (py-truthy (py-eq @iterations (py-neg 1))) (do (println (clojure.string/join " " [(py-str (str "  ❌ Did not reach 6174. Sequence: " (py-str @sequence)))]))) (do (if (py-truthy (py-eq @iterations 0)) (do (println (clojure.string/join " " [(py-str "  ✓ Already at Kaprekar's constant!")]))) (do (println (clojure.string/join " " [(py-str (str "  ✓ Reached 6174 in " (py-str @iterations) " iteration(s)"))])) (println (clojure.string/join " " [(py-str (str "  Sequence: " (py-str (clojure.string/join " -> " (map py-str (doall (for [n (py-iter @sequence)] (py-str n))))))))])))))) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e))))) (println (clojure.string/join " " [(py-str (py-add "\n" (py-mul "=" 50)))])) (println (clojure.string/join " " [(py-str "\nTry your own number (0-9999):")])) (try (reset! user-input (py-int (do (print "Enter a 4-digit number: ") (flush) (read-line)) nil)) (if (py-truthy (let [__c3 0 __c4 @user-input __c5 9999] (and (py-le __c3 __c4) (py-le __c4 __c5)))) (do (if (py-truthy (not (py-truthy (is-kaprekar-valid @user-input)))) (do (println (clojure.string/join " " [(py-str "Invalid: All digits are the same!")]))) (do (let [__d6 (vec (kaprekar-constant @user-input))] (reset! iterations (nth __d6 0)) (reset! sequence (nth __d6 1))) (println (clojure.string/join " " [(py-str (str "\nSequence: " (py-str (clojure.string/join " -> " (map py-str (doall (for [n (py-iter @sequence)] (py-str n))))))))])) (when (py-truthy (not (py-eq @iterations (py-neg 1)))) (println (clojure.string/join " " [(py-str (str "Reached 6174 in " (py-str @iterations) " iteration(s)!"))])))))) (do (println (clojure.string/join " " [(py-str "Number must be between 0 and 9999")])))) (catch clojure.lang.ExceptionInfo __eh (if (:__control__ (ex-data __eh)) (throw __eh) (let [__t (:type (ex-data __eh))] (cond (contains? #{:EOFError :ValueError} __t) (do (println (clojure.string/join " " [(py-str "Invalid input or no input provided")]))) :else (throw __eh)))))) nil))
