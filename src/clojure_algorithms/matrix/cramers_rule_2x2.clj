(ns clojure-algorithms.matrix.cramers-rule-2x2
  "Port of matrix/cramers_rule_2x2.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare cramers-rule-2x2)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-index-norm py-get py-sub py-repeat-vec py-mul py-div py-float-str py-quote py-repr py-str)

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

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

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

(defn cramers-rule-2x2 "Solves the system of linear equation in 2 variables.\n:param: equation1: list of 3 numbers\n:param: equation2: list of 3 numbers\n:return: String of result\ninput format : [a1, b1, d1], [a2, b2, d2]\ndeterminant = [[a1, b1], [a2, b2]]\ndeterminant_x = [[d1, b1], [d2, b2]]\ndeterminant_y = [[a1, d1], [a2, d2]]\n\n>>> cramers_rule_2x2([2, 3, 0], [5, 1, 0])\n(0.0, 0.0)\n>>> cramers_rule_2x2([0, 4, 50], [2, 0, 26])\n(13.0, 12.5)\n>>> cramers_rule_2x2([11, 2, 30], [1, 0, 4])\n(4.0, -7.0)\n>>> cramers_rule_2x2([4, 7, 1], [1, 2, 0])\n(2.0, -1.0)\n\n>>> cramers_rule_2x2([1, 2, 3], [2, 4, 6])\nTraceback (most recent call last):\n    ...\nValueError: Infinite solutions. (Consistent system)\n>>> cramers_rule_2x2([1, 2, 3], [2, 4, 7])\nTraceback (most recent call last):\n    ...\nValueError: No solution. (Inconsistent system)\n>>> cramers_rule_2x2([1, 2, 3], [11, 22])\nTraceback (most recent call last):\n    ...\nValueError: Please enter a valid equation.\n>>> cramers_rule_2x2([0, 1, 6], [0, 0, 3])\nTraceback (most recent call last):\n    ...\nValueError: No solution. (Inconsistent system)\n>>> cramers_rule_2x2([0, 0, 6], [0, 0, 3])\nTraceback (most recent call last):\n    ...\nValueError: Both a & b of two equations can't be zero.\n>>> cramers_rule_2x2([1, 2, 3], [1, 2, 3])\nTraceback (most recent call last):\n    ...\nValueError: Infinite solutions. (Consistent system)\n>>> cramers_rule_2x2([0, 4, 50], [0, 3, 99])\nTraceback (most recent call last):\n    ...\nValueError: No solution. (Inconsistent system)"
  [equation1 equation2] (let [a1 (atom nil) a2 (atom nil) b1 (atom nil) b2 (atom nil) c1 (atom nil) c2 (atom nil) determinant (atom nil) determinant-x (atom nil) determinant-y (atom nil) x (atom nil) y (atom nil)] (try (when (py-truthy (not (py-truthy (let [__c1 (count equation1) __c2 (count equation2) __c3 3] (and (py-eq __c1 __c2) (py-eq __c2 __c3)))))) (throw (ex-info (py-str "Please enter a valid equation.") {:type :ValueError}))) (when (py-truthy (let [__c4 (py-get equation1 0) __c5 (py-get equation1 1) __c6 (py-get equation2 0) __c7 (py-get equation2 1) __c8 0] (and (py-eq __c4 __c5) (py-eq __c5 __c6) (py-eq __c6 __c7) (py-eq __c7 __c8)))) (throw (ex-info (py-str "Both a & b of two equations can't be zero.") {:type :ValueError}))) (let [__d9 (vec equation1)] (reset! a1 (nth __d9 0)) (reset! b1 (nth __d9 1)) (reset! c1 (nth __d9 2))) (let [__d10 (vec equation2)] (reset! a2 (nth __d10 0)) (reset! b2 (nth __d10 1)) (reset! c2 (nth __d10 2))) (reset! determinant (py-sub (py-mul @a1 @b2) (py-mul @a2 @b1))) (reset! determinant-x (py-sub (py-mul @c1 @b2) (py-mul @c2 @b1))) (reset! determinant-y (py-sub (py-mul @a1 @c2) (py-mul @a2 @c1))) (when (py-truthy (py-eq @determinant 0)) (when (py-truthy (let [__c11 @determinant-x __c12 @determinant-y __c13 0] (and (py-eq __c11 __c12) (py-eq __c12 __c13)))) (throw (ex-info (py-str "Infinite solutions. (Consistent system)") {:type :ValueError}))) (throw (ex-info (py-str "No solution. (Inconsistent system)") {:type :ValueError}))) (if (py-truthy (let [__c14 @determinant-x __c15 @determinant-y __c16 0] (and (py-eq __c14 __c15) (py-eq __c15 __c16)))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value [0.0 0.0]}))) (do (reset! x (py-div @determinant-x @determinant)) (reset! y (py-div @determinant-y @determinant)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@x @y]})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
