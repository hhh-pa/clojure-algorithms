(ns clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx
  "Author : Syed Faizan ( 3rd Year IIIT Pune )\nGithub : faizan2700\n\nPurpose : You have one function f(x) which takes float integer and returns\nfloat you have to integrate the function in limits a to b.\nThe approximation proposed by Thomas Simpson in 1743 is one way to calculate\nintegration.\n\n( read article : https://cp-algorithms.com/num_methods/simpson-integration.html )\n\nsimpson_integration() takes function,lower_limit=a,upper_limit=b,precision and\nreturns the integration of function in given limit."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare f simpson-integration N-STEPS)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-repeat-vec py-mul py-div py-mod py-float-str py-quote py-repr py-str py-round py-type)

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

(defn- py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

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

(defn f
  [x] (py-mul x x))

(defn simpson-integration "Args:\n    function : the function which's integration is desired\n    a : the lower limit of integration\n    b : upper limit of integration\n    precision : precision of the result,error required default is 4\n\nReturns:\n    result : the value of the approximated integration of function in range a to b\n\nRaises:\n    AssertionError: function is not callable\n    AssertionError: a is not float or integer\n    AssertionError: function should return float or integer\n    AssertionError: b is not float or integer\n    AssertionError: precision is not positive integer\n\n>>> simpson_integration(lambda x : x*x,1,2,3)\n2.333\n\n>>> simpson_integration(lambda x : x*x,'wrong_input',2,3)\nTraceback (most recent call last):\n    ...\nAssertionError: a should be float or integer your input : wrong_input\n\n>>> simpson_integration(lambda x : x*x,1,'wrong_input',3)\nTraceback (most recent call last):\n    ...\nAssertionError: b should be float or integer your input : wrong_input\n\n>>> simpson_integration(lambda x : x*x,1,2,'wrong_input')\nTraceback (most recent call last):\n    ...\nAssertionError: precision should be positive integer your input : wrong_input\n>>> simpson_integration('wrong_input',2,3,4)\nTraceback (most recent call last):\n    ...\nAssertionError: the function(object) passed should be callable your input : ...\n\n>>> simpson_integration(lambda x : x*x,3.45,3.2,1)\n-2.8\n\n>>> simpson_integration(lambda x : x*x,3.45,3.2,0)\nTraceback (most recent call last):\n    ...\nAssertionError: precision should be positive integer your input : 0\n\n>>> simpson_integration(lambda x : x*x,3.45,3.2,-1)\nTraceback (most recent call last):\n    ...\nAssertionError: precision should be positive integer your input : -1"
  ([function a b] (simpson-integration function a b 4))
  ([function a b precision] (let [a1 (atom nil) h (atom nil) i (atom nil) result (atom nil)] (try (assert (py-truthy (fn? function)) (py-str (str "the function(object) passed should be callable your input : " (py-str function)))) (assert (py-truthy (or (number? a) (integer? a))) (py-str (str "a should be float or integer your input : " (py-str a)))) (assert (py-truthy (or (number? (function a)) (integer? (function a)))) (py-str (str "the function should return integer or float return type of your function, " (py-str (py-type a))))) (assert (py-truthy (or (number? b) (integer? b))) (py-str (str "b should be float or integer your input : " (py-str b)))) (assert (py-truthy (let [__b1 (integer? precision)] (if (py-truthy __b1) (py-gt precision 0) __b1))) (py-str (str "precision should be positive integer your input : " (py-str precision)))) (reset! h (py-div (py-sub b a) N-STEPS)) (reset! result (py-add (function a) (function b))) (doseq [__i2 (py-iter (range 1 N-STEPS))] (reset! i __i2) (reset! a1 (py-add a (py-mul @h @i))) (swap! result py-add (py-mul (function @a1) (if (py-truthy (py-mod @i 2)) 4 2)))) (swap! result py-mul (py-div @h 3)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-round @result precision)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(def N-STEPS 1000)
