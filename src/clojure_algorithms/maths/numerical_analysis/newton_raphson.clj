(ns clojure-algorithms.maths.numerical-analysis.newton-raphson
  "The Newton-Raphson method (aka the Newton method) is a root-finding algorithm that\napproximates a root of a given real-valued function f(x). It is an iterative method\ngiven by the formula\n\nx_{n + 1} = x_n + f(x_n) / f'(x_n)\n\nwith the precision of the approximation increasing as the number of iterations increase.\n\nReference: https://en.wikipedia.org/wiki/Newton%27s_method"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare calc-derivative newton-raphson RealFunc)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-div py-abs py-float-str py-quote py-repr py-str)

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

(defn- py-abs [x]
  (cond (boolean? x) (if x 1 0)
        (number? x) (if (neg? x) (-' x) x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :abs x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for abs()" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for abs()" {:type :TypeError}))))

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

(defn calc-derivative "Approximate the derivative of a function f(x) at a point x using the finite\ndifference method\n\n>>> import math\n>>> tolerance = 1e-5\n>>> derivative = calc_derivative(lambda x: x**2, 2)\n>>> math.isclose(derivative, 4, abs_tol=tolerance)\nTrue\n>>> derivative = calc_derivative(math.sin, 0)\n>>> math.isclose(derivative, 1, abs_tol=tolerance)\nTrue"
  ([f x] (calc-derivative f x 0.001))
  ([f x delta-x] (py-div (py-sub (f (py-add x (py-div delta-x 2))) (f (py-sub x (py-div delta-x 2)))) delta-x)))

(defn newton-raphson "Find a root of the given function f using the Newton-Raphson method.\n\n:param f: A real-valued single-variable function\n:param x0: Initial guess\n:param max_iter: Maximum number of iterations\n:param step: Step size of x, used to approximate f'(x)\n:param max_error: Maximum approximation error\n:param log_steps: bool denoting whether to log intermediate steps\n\n:return: A tuple containing the approximation, the error, and the intermediate\n    steps. If log_steps is False, then an empty list is returned for the third\n    element of the tuple.\n\n:raises ZeroDivisionError: The derivative approaches 0.\n:raises ArithmeticError: No solution exists, or the solution isn't found before the\n    iteration limit is reached.\n\n>>> import math\n>>> tolerance = 1e-15\n>>> root, *_ = newton_raphson(lambda x: x**2 - 5*x + 2, 0.4, max_error=tolerance)\n>>> math.isclose(root, (5 - math.sqrt(17)) / 2, abs_tol=tolerance)\nTrue\n>>> root, *_ = newton_raphson(lambda x: math.log(x) - 1, 2, max_error=tolerance)\n>>> math.isclose(root, math.e, abs_tol=tolerance)\nTrue\n>>> root, *_ = newton_raphson(math.sin, 1, max_error=tolerance)\n>>> math.isclose(root, 0, abs_tol=tolerance)\nTrue\n>>> newton_raphson(math.cos, 0)\nTraceback (most recent call last):\n...\nZeroDivisionError: No converging solution found, zero derivative\n>>> newton_raphson(lambda x: x**2 + 1, 2)\nTraceback (most recent call last):\n...\nArithmeticError: No converging solution found, iteration limit reached"
  ([f] (newton-raphson f 0 100 1e-06 1e-06 false))
  ([f x0] (newton-raphson f x0 100 1e-06 1e-06 false))
  ([f x0 max-iter] (newton-raphson f x0 max-iter 1e-06 1e-06 false))
  ([f x0 max-iter step] (newton-raphson f x0 max-iter step 1e-06 false))
  ([f x0 max-iter step max-error] (newton-raphson f x0 max-iter step max-error false))
  ([f x0 max-iter step max-error log-steps] (let [--v (atom nil) a (atom nil) error (atom nil) steps (atom nil)] (try (letfn [(f-derivative [x] (calc-derivative f x step))] (reset! a x0) (reset! steps []) (doseq [__i1 (py-iter (range max-iter))] (reset! --v __i1) (when (py-truthy log-steps) (swap! steps conj @a)) (reset! error (py-abs (f @a))) (when (py-truthy (py-lt @error max-error)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@a @error @steps]}))) (when (py-truthy (py-eq (f-derivative @a) 0)) (throw (ex-info (py-str "No converging solution found, zero derivative") {:type :ZeroDivisionError}))) (swap! a py-sub (py-div (f @a) (f-derivative @a)))) (throw (ex-info (py-str "No converging solution found, iteration limit reached") {:type :ArithmeticError}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(def RealFunc nil)

;; NOTE: const RealFunc is a typing alias
