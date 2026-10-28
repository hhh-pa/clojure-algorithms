(ns clojure-algorithms.maths.numerical-analysis.brent-method
  "Port of maths/numerical_analysis/brent_method.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare brent-method)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-repeat-vec py-mul py-div py-abs py-float-str py-quote py-repr py-str)

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

(defn brent-method "Find the root of function func in the interval [left, right] using Brent's Method.\n\nBrent's Method combines bisection, secant, and inverse quadratic interpolation.\n\n\nParameters\n----------\nfunc : Callable[[float], float]\n    Function for which to find the root.\nleft : float\n    Left endpoint of interval.\nright : float\n    Right endpoint of interval.\ntol : float\n    Tolerance for convergence (default 1e-8).\nmax_iter : int\n    Maximum number of iterations (default 100).\n\n\nReturns\n-------\nfloat\n    Approximate root of func in [left, right].\n\nRaises\n------\nValueError\n    If func(left) and func(right) do not have opposite signs.\n\nExamples\n--------\n>>> def f(x): return x**3 - x - 2\n>>> round(brent_method(f, 1, 2), 5)\n1.52138\n\n>>> def f2(x): return x**2 + 1\n>>> brent_method(f2, 0, 1)\nTraceback (most recent call last):\n...\nValueError: func(left) and func(right) must have opposite signs"
  ([func left right] (brent-method func left right 1e-08 100))
  ([func left right tol] (brent-method func left right tol 100))
  ([func left right tol max-iter] (let [left (atom left) right (atom right)] (let [c (atom nil) conditions (atom nil) d (atom nil) fc (atom nil) fl (atom nil) fr (atom nil) fs (atom nil) iteration (atom nil) s (atom nil)] (try (reset! fl (func @left)) (reset! fr (func @right)) (when (py-truthy (py-ge (py-mul @fl @fr) 0)) (throw (ex-info (py-str "func(left) and func(right) must have opposite signs") {:type :ValueError}))) (when (py-truthy (py-lt (py-abs @fl) (py-abs @fr))) (let [__d1 (vec [@right @left])] (reset! left (nth __d1 0)) (reset! right (nth __d1 1))) (let [__d2 (vec [@fr @fl])] (reset! fl (nth __d2 0)) (reset! fr (nth __d2 1)))) (reset! c @left) (reset! fc @fl) (reset! d (py-sub @right @left)) (doseq [__i3 (py-iter (range max-iter))] (reset! iteration __i3) (when (py-truthy (py-eq @fr 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @right}))) (if (py-truthy (not (py-in @fc [@fl @fr]))) (do (reset! s (py-add (py-add (py-div (py-mul (py-mul @left @fr) @fc) (py-mul (py-sub @fl @fr) (py-sub @fl @fc))) (py-div (py-mul (py-mul @right @fl) @fc) (py-mul (py-sub @fr @fl) (py-sub @fr @fc)))) (py-div (py-mul (py-mul @c @fl) @fr) (py-mul (py-sub @fc @fl) (py-sub @fc @fr)))))) (do (reset! s (py-sub @right (py-div (py-mul @fr (py-sub @right @left)) (py-sub @fr @fl)))))) (reset! conditions [(if (py-truthy (py-gt @right @left)) (not (py-truthy (let [__c4 (py-div (py-add (py-mul 3 @left) @right) 4) __c5 @s __c6 @right] (and (py-lt __c4 __c5) (py-lt __c5 __c6))))) (not (py-truthy (let [__c7 @right __c8 @s __c9 (py-div (py-add (py-mul 3 @left) @right) 4)] (and (py-lt __c7 __c8) (py-lt __c8 __c9)))))) (let [__b10 (py-gt @iteration 1)] (if (py-truthy __b10) (py-ge (py-abs (py-sub @s @right)) (py-div (py-abs (py-sub @right @c)) 2)) __b10)) (let [__b11 (py-le @iteration 1)] (if (py-truthy __b11) (py-ge (py-abs (py-sub @s @right)) (py-div (py-abs (py-sub @c @d)) 2)) __b11)) (let [__b12 (py-gt @iteration 1)] (if (py-truthy __b12) (py-lt (py-abs (py-sub @right @c)) tol) __b12)) (let [__b13 (py-le @iteration 1)] (if (py-truthy __b13) (py-lt (py-abs (py-sub @c @d)) tol) __b13))]) (when (py-truthy (boolean (some py-truthy @conditions))) (reset! s (py-div (py-add @left @right) 2)) (reset! d (py-sub @right @left))) (reset! fs (func @s)) (let [__d14 (vec [@c @right])] (reset! d (nth __d14 0)) (reset! c (nth __d14 1))) (reset! fc @fr) (if (py-truthy (py-lt (py-mul @fl @fs) 0)) (do (reset! right @s) (reset! fr @fs)) (do (reset! left @s) (reset! fl @fs))) (when (py-truthy (py-lt (py-abs @fl) (py-abs @fr))) (let [__d15 (vec [@right @left])] (reset! left (nth __d15 0)) (reset! right (nth __d15 1))) (let [__d16 (vec [@fr @fl])] (reset! fl (nth __d16 0)) (reset! fr (nth __d16 1)))) (when (py-truthy (py-lt (py-abs (py-sub @right @left)) tol)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @right})))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @right})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))))
