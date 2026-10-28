(ns clojure-algorithms.maths.josephus-problem
  "The Josephus problem is a famous theoretical problem related to a certain\ncounting-out game. This module provides functions to solve the Josephus problem\nfor num_people and a step_size.\n\nThe Josephus problem is defined as follows:\n- num_people are standing in a circle.\n- Starting with a specified person, you count around the circle,\n  skipping a fixed number of people (step_size).\n- The person at which you stop counting is eliminated from the circle.\n- The counting continues until only one person remains.\n\nFor more information about the Josephus problem, refer to:\nhttps://en.wikipedia.org/wiki/Josephus_problem"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare josephus-recursive find-winner josephus-iterative)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-mod py-list py-pop2 py-pop-rest py-float-str py-quote py-repr py-str)

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

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-pop2
  ([c k] (py-pop2 c k :py-no-default))
  ([c k dflt]
   (cond (map? c) (if (contains? c k) [(get c k) (dissoc c k)]
                      (if (= dflt :py-no-default)
                        (throw (ex-info (str "key: " k) {:type :KeyError})) [dflt c]))
         (set? c) (if (nil? k)
                    (if (empty? c)
                      (throw (ex-info "pop from empty set" {:type :KeyError}))
                      [(first c) (disj c (first c))])
                    (if (contains? c k) [k (disj c k)]
                        (throw (ex-info (str "key: " k) {:type :KeyError}))))
         :else (let [v (vec c)]
                 (if (nil? k)
                   (if (empty? v)
                     (throw (ex-info "pop from empty list" {:type :IndexError}))
                     [(peek v) (pop v)])
                   (let [i (py-index-norm k (count v))]
                     [(nth v i) (vec (concat (subvec v 0 i) (subvec v (inc i))))]))))))

(defn- py-pop-rest
  ([c k] (py-pop-rest c k :py-no-default))
  ([c k dflt] (second (py-pop2 c k dflt))))

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

(defn josephus-recursive "Solve the Josephus problem for num_people and a step_size recursively.\n\nArgs:\n    num_people: A positive integer representing the number of people.\n    step_size: A positive integer representing the step size for elimination.\n\nReturns:\n    The position of the last person remaining.\n\nRaises:\n    ValueError: If num_people or step_size is not a positive integer.\n\nExamples:\n    >>> josephus_recursive(7, 3)\n    3\n    >>> josephus_recursive(10, 2)\n    4\n    >>> josephus_recursive(0, 2)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(1.9, 2)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(-2, 2)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(7, 0)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(7, -2)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(1_000, 0.01)\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer.\n    >>> josephus_recursive(\"cat\", \"dog\")\n    Traceback (most recent call last):\n        ...\n    ValueError: num_people or step_size is not a positive integer."
  [num-people step-size] (try (when (py-truthy (let [__b3 (not (py-truthy (integer? num-people)))] (if (py-truthy __b3) __b3 (let [__b2 (not (py-truthy (integer? step-size)))] (if (py-truthy __b2) __b2 (let [__b1 (py-le num-people 0)] (if (py-truthy __b1) __b1 (py-le step-size 0)))))))) (throw (ex-info (py-str "num_people or step_size is not a positive integer.") {:type :ValueError}))) (when (py-truthy (py-eq num-people 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-add (josephus-recursive (py-sub num-people 1) step-size) step-size) num-people)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn find-winner "Find the winner of the Josephus problem for num_people and a step_size.\n\nArgs:\n    num_people (int): Number of people.\n    step_size (int): Step size for elimination.\n\nReturns:\n    int: The position of the last person remaining (1-based index).\n\nExamples:\n    >>> find_winner(7, 3)\n    4\n    >>> find_winner(10, 2)\n    5"
  [num-people step-size] (py-add (josephus-recursive num-people step-size) 1))

(defn josephus-iterative "Solve the Josephus problem for num_people and a step_size iteratively.\n\nArgs:\n    num_people (int): The number of people in the circle.\n    step_size (int): The number of steps to take before eliminating someone.\n\nReturns:\n    int: The position of the last person standing.\n\nExamples:\n    >>> josephus_iterative(5, 2)\n    3\n    >>> josephus_iterative(7, 3)\n    4"
  [num-people step-size] (let [circle (atom nil) current (atom nil)] (try (reset! circle (py-list (range 1 (py-add num-people 1)))) (reset! current 0) (while (py-truthy (py-gt (count @circle) 1)) (reset! current (py-mod (py-sub (py-add @current step-size) 1) (count @circle))) (swap! circle py-pop-rest @current)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @circle 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
