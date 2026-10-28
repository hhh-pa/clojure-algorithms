(ns clojure-algorithms.maths.line-length
  "Port of maths/line_length.py"
  (:require [clojure.set :as set]))

(declare line-length)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-add py-sub py-div)

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

(defn line-length "Approximates the arc length of a line segment by treating the curve as a\nsequence of linear lines and summing their lengths\n:param fnc: a function which defines a curve\n:param x_start: left end point to indicate the start of line segment\n:param x_end: right end point to indicate end of line segment\n:param steps: an accuracy gauge; more steps increases accuracy\n:return: a float representing the length of the curve\n\n>>> def f(x):\n...    return x\n>>> f\"{line_length(f, 0, 1, 10):.6f}\"\n'1.414214'\n\n>>> def f(x):\n...    return 1\n>>> f\"{line_length(f, -5.5, 4.5):.6f}\"\n'10.000000'\n\n>>> def f(x):\n...    return math.sin(5 * x) + math.cos(10 * x) + x * x/10\n>>> f\"{line_length(f, 0.0, 10.0, 10000):.6f}\"\n'69.534930'"
  ([fnc x-start x-end] (line-length fnc x-start x-end 100))
  ([fnc x-start x-end steps] (let [--v (atom nil) fx1 (atom nil) fx2 (atom nil) length (atom nil) x1 (atom nil) x2 (atom nil)] (try (reset! x1 x-start) (reset! fx1 (fnc x-start)) (reset! length 0.0) (doseq [__i1 (py-iter (range steps))] (reset! --v __i1) (reset! x2 (py-add (py-div (py-sub x-end x-start) steps) @x1)) (reset! fx2 (fnc @x2)) (swap! length py-add (Math/hypot (py-sub @x2 @x1) (py-sub @fx2 @fx1))) (reset! x1 @x2) (reset! fx1 @fx2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @length})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
