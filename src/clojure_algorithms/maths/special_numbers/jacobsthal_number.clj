(ns clojure-algorithms.maths.special-numbers.jacobsthal-number
  "Jacobsthal numbers\n\nThe Jacobsthal sequence starts with 0 and 1, and every later term is the\nprevious term plus twice the term before it:\n\n    J(0) = 0\n    J(1) = 1\n    J(n) = J(n - 1) + 2 * J(n - 2)\n\nThe first terms are 0, 1, 1, 3, 5, 11, 21, 43, 85, 171, 341, ...\n\nThey also have the closed form J(n) = (2 ** n - (-1) ** n) / 3, which is used\nbelow to cross-check the iterative result.\n\nSource:\n    https://en.wikipedia.org/wiki/Jacobsthal_number\n    https://oeis.org/A001045"
  (:require [clojure.string :as str]))

(declare jacobsthal-number jacobsthal-sequence)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-repeat-vec py-mul py-float-str py-quote py-repr py-str)

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

(defn jacobsthal-number "Return the nth Jacobsthal number, counting from J(0) = 0.\n\n:param n: index of the Jacobsthal number, must be a non-negative integer\n:return: the nth Jacobsthal number\n\n>>> jacobsthal_number(0)\n0\n>>> jacobsthal_number(1)\n1\n>>> jacobsthal_number(5)\n11\n>>> jacobsthal_number(10)\n341\n>>> jacobsthal_number(64)\n6148914691236517205\n>>> all(\n...     jacobsthal_number(i) == (2**i - (-1) ** i) // 3 for i in range(100)\n... )\nTrue\n>>> jacobsthal_number(-1)\nTraceback (most recent call last):\n    ...\nValueError: n must be a non-negative integer, got -1\n>>> jacobsthal_number(2.5)\nTraceback (most recent call last):\n    ...\nValueError: n must be a non-negative integer, got 2.5\n>>> jacobsthal_number(\"3\")\nTraceback (most recent call last):\n    ...\nValueError: n must be a non-negative integer, got '3'"
  [n] (let [--v (atom nil) current (atom nil) msg (atom nil) previous (atom nil)] (try (when (py-truthy (let [__b2 (not (py-truthy (integer? n)))] (if (py-truthy __b2) __b2 (let [__b1 (boolean? n)] (if (py-truthy __b1) __b1 (py-lt n 0)))))) (reset! msg (str "n must be a non-negative integer, got " (py-str (pr-str n)))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (let [__d3 (vec [0 1])] (reset! previous (nth __d3 0)) (reset! current (nth __d3 1))) (doseq [__i4 (py-iter (range n))] (reset! --v __i4) (let [__d5 (vec [@current (py-add @current (py-mul 2 @previous))])] (reset! previous (nth __d5 0)) (reset! current (nth __d5 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @previous})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn jacobsthal-sequence "Return the first `length` Jacobsthal numbers as a list.\n\n:param length: how many terms to return, must be a non-negative integer\n:return: list of the first `length` Jacobsthal numbers\n\n>>> jacobsthal_sequence(0)\n[]\n>>> jacobsthal_sequence(1)\n[0]\n>>> jacobsthal_sequence(11)\n[0, 1, 1, 3, 5, 11, 21, 43, 85, 171, 341]\n>>> jacobsthal_sequence(-3)\nTraceback (most recent call last):\n    ...\nValueError: length must be a non-negative integer, got -3\n>>> jacobsthal_sequence(4.0)\nTraceback (most recent call last):\n    ...\nValueError: length must be a non-negative integer, got 4.0"
  [length] (let [--v (atom nil) current (atom nil) msg (atom nil) previous (atom nil) sequence (atom nil)] (try (when (py-truthy (let [__b2 (not (py-truthy (integer? length)))] (if (py-truthy __b2) __b2 (let [__b1 (boolean? length)] (if (py-truthy __b1) __b1 (py-lt length 0)))))) (reset! msg (str "length must be a non-negative integer, got " (py-str (pr-str length)))) (throw (ex-info (py-str @msg) {:type :ValueError}))) (reset! sequence []) (let [__d3 (vec [0 1])] (reset! previous (nth __d3 0)) (reset! current (nth __d3 1))) (doseq [__i4 (py-iter (range length))] (reset! --v __i4) (swap! sequence conj @previous) (let [__d5 (vec [@current (py-add @current (py-mul 2 @previous))])] (reset! previous (nth __d5 0)) (reset! current (nth __d5 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @sequence})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
