(ns clojure-algorithms.maths.numerical-analysis.nth-root
  "Approximate the nth root of a real number using Newton's Method.\n\nThe nth root of a real number R can be computed with Newton's method,\nwhich starts with an initial guess x_0 and then iterates using the\nrecurrence relation:\n\nx_{k + 1} = x_k - ((x_k)**n - R)/(n*(x_k)**(n-1))\n\nThe recurrence relation can be rewritten for computational efficiency:\n\nx_{k + 1} = (n-1)/n*x_k + R/(n*(x_k)**(n-1))\n\nGiven a tolerance TOL, a stopping criterion can be set as:\n\nabs(x_{k + 1} - x_k) < TOL\n\nReferences:\n- https://en.wikipedia.org/wiki/Nth_root#Using_Newton's_method\n- Sauer, T. (2011): Numerical analysis.\n    USA. Addison-Wesley Publishing Company."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare nth-root)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-add py-sub py-repeat-vec py-mul py-div py-mod py-pow py-abs py-float-str py-quote py-repr py-str py-type)

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

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

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

(defn- py-type [x]
  (cond (nil? x) :none-type
        (boolean? x) :bool
        (integer? x) :int
        (float? x) :float
        (string? x) :str
        (set? x) :set
        (map? x) :dict
        (sequential? x) :list
        :else (class x)))

(defn nth-root "Approximate the nth root of the radicand for the given index\n\nArgs:\n    radicand: number from which the root is taken\n    index: positive integer which is the degree of the root\n    tolerance: positive real number that establishes the stopping criterion\n\nReturns:\n    new_approximation: approximation of the nth root of the radicand for the\n    given index\n\nRaises:\n    TypeError: radicand is not a real number\n    TypeError: index is not an integer\n    ValueError: index is not a positive integer\n    TypeError: tolerance is not a real number\n    ValueError: tolerance is not a positive real number\n    ValueError: math domain error\n\n>>> round(nth_root(9, 2),1)\n3.0\n\n>>> int(round(nth_root(-8, 3, 0.001)))\n-2\n\n>>> int(round(nth_root(256, 4, 0.001)))\n4\n\n>>> round(nth_root(2, 2), 5)\n1.41421\n\n>>> round(nth_root(0.25, 2, 0.00000001), 1)\n0.5\n\n>>> round(nth_root(-8/27, 3, 0.0000001), 5)\n-0.66667\n\n>>> nth_root(0, 2, 0.1)\n0.0\n\n>>> nth_root(0.0, 5)\n0.0\n\n>>> all(abs(nth_root(k, k, 0.00000001) - k**(1/k)) <= 1e-10 for k in range(1,10))\nTrue\n\n>>> nth_root('invalid input', 3, 0.0001)\nTraceback (most recent call last):\n    ...\nTypeError: radicand must be a real number, not a str\n\n>>> nth_root(4, 0.5, 0.0001)\nTraceback (most recent call last):\n    ...\nTypeError: index must be an integer, not a float\n\n>>> nth_root(16, -4, 0.001)\nTraceback (most recent call last):\n    ...\nValueError: index must be a positive integer, -4 <= 0\n\n>>> nth_root(4, 2, '0.000001')\nTraceback (most recent call last):\n    ...\nTypeError: tolerance must be a real number, not str\n\n>>> nth_root(9, 2, -0.01)\nTraceback (most recent call last):\n    ...\nValueError: tolerance must be a positive real number, -0.01 <= 0\n\n>>> nth_root(-256, 4, 0.0001)\nTraceback (most recent call last):\n    ...\nValueError: math domain error, radicand must be nonnegative for even index"
  ([radicand index] (nth-root radicand index 0.0001))
  ([radicand index tolerance] (let [error-message (atom nil) first-summand (atom nil) new-approximation (atom nil) old-approximation (atom nil) second-summand (atom nil)] (try (when (py-truthy (not (py-truthy (or (integer? radicand) (number? radicand))))) (reset! error-message (str "radicand must be a real number, not a " (py-str (:--name-- (py-type radicand))))) (throw (ex-info (py-str @error-message) {:type :TypeError}))) (when (py-truthy (not (py-truthy (integer? index)))) (reset! error-message (str "index must be an integer, not a " (py-str (:--name-- (py-type index))))) (throw (ex-info (py-str @error-message) {:type :TypeError}))) (when (py-truthy (py-le index 0)) (reset! error-message (str "index must be a positive integer, " (py-str index) " <= 0")) (throw (ex-info (py-str @error-message) {:type :ValueError}))) (when (py-truthy (not (py-truthy (or (integer? tolerance) (number? tolerance))))) (reset! error-message (str "tolerance must be a real number, not " (py-str (:--name-- (py-type tolerance))))) (throw (ex-info (py-str @error-message) {:type :TypeError}))) (when (py-truthy (py-le tolerance 0)) (reset! error-message (str "tolerance must be a positive real number, " (py-str tolerance) " <= 0")) (throw (ex-info (py-str @error-message) {:type :ValueError}))) (when (py-truthy (let [__b1 (py-lt radicand 0)] (if (py-truthy __b1) (py-eq (py-mod index 2) 0) __b1))) (reset! error-message "math domain error, radicand must be nonnegative for even index") (throw (ex-info (py-str @error-message) {:type :ValueError}))) (when (py-truthy (py-eq radicand 0.0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0.0}))) (reset! new-approximation radicand) (reset! old-approximation (py-add (py-add @new-approximation tolerance) 0.1)) (while (py-truthy (py-le tolerance (py-abs (py-sub @old-approximation @new-approximation)))) (reset! old-approximation @new-approximation) (reset! first-summand (py-mul (py-div (py-sub index 1) index) @old-approximation)) (reset! second-summand (py-div radicand (py-mul index (py-pow @old-approximation (py-sub index 1))))) (reset! new-approximation (py-add @first-summand @second-summand))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @new-approximation})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
