(ns clojure-algorithms.maths.weddles-rule
  "Port of maths/weddles_rule.py"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare get-inputs safe-function-eval compute-table apply-weights compute-solution)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-repeat-vec py-mul py-mod py-neg py-float-str py-quote py-repr py-float)

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

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

(defn- py-float [x]
  (cond (boolean? x) (if x 1.0 0.0)
        (integer? x) (double x)
        (float? x) (double x)
        (string? x)
        (let [s (-> x clojure.string/trim (clojure.string/replace "_" ""))
              l (clojure.string/lower-case s)]
          (cond (contains? #{"inf" "+inf" "infinity" "+infinity"} l) Double/POSITIVE_INFINITY
                (contains? #{"-inf" "-infinity"} l) Double/NEGATIVE_INFINITY
                (contains? #{"nan" "+nan" "-nan"} l) Double/NaN
                :else (try (Double/parseDouble s)
                           (catch NumberFormatException e
                             (throw (ex-info (str "could not convert string to float: " x) {:type :ValueError}))))))
        :else (throw (ex-info "float() argument must be a string or a number" {:type :TypeError}))))

(defn get-inputs "Get user input for the function, lower limit, and upper limit.\n\nReturns:\n    Tuple[str, float, float]: A tuple containing the function as a string,\n    the lower limit (a), and the upper limit (b) as floats.\n\nExample:\n    >>> from unittest.mock import patch\n    >>> inputs = ['1/(1+x**2)', 1.0, -1.0]\n    >>> with patch('builtins.input', side_effect=inputs):\n    ...     get_inputs()\n    ('1/(1+x**2)', 1.0, -1.0)"
  [] (let [func (atom nil) lower-limit (atom nil) upper-limit (atom nil)] (try (reset! func (do (print "Enter function with variable as x: ") (flush) (read-line))) (reset! lower-limit (py-float (do (print "Enter lower limit: ") (flush) (read-line)))) (reset! upper-limit (py-float (do (print "Enter upper limit: ") (flush) (read-line)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@func @lower-limit @upper-limit]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn safe-function-eval "Safely evaluates the function by substituting x value using sympy.\n\nArgs:\n    func_str (str): Function expression as a string.\n\nReturns:\n    Callable: A callable lambda function for numerical evaluation.\n\nExamples:\n    >>> f = safe_function_eval('x**2')\n    >>> f(3)\n    9\n    >>> f = safe_function_eval('x + x**2')\n    >>> f(2)\n    6"
  [& _args]
  ;; stub: original requires lambdify,symbols,sympify; pure-Clojure port not applicable
  (throw (ex-info "requires lambdify,symbols,sympify" {:type :UnsupportedOperation})))

(defn compute-table "Compute the table of function values based on the limits and accuracy.\n\nArgs:\n    func (Callable): The mathematical function as a callable.\n    lower_limit (float): The lower limit of the integral.\n    upper_limit (float): The upper limit of the integral.\n    acc (int): The number of subdivisions for accuracy.\n\nReturns:\n    Tuple[np.ndarray, float]: A tuple containing the table\n    of values and the step size (h).\n\nExample:\n    >>> compute_table(\n    ...     safe_function_eval('1/(1+x**2)'), 1, -1, 1\n    ... )\n    (array([0.5       , 0.69230769, 0.9       , 1.        , 0.9       ,\n           0.69230769, 0.5       ]), -0.3333333333333333)"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn apply-weights "Apply Weddle's rule weights to the values in the table.\n\nArgs:\n    table (List[float]): A list of computed function values.\n\nReturns:\n    List[float]: A list of weighted values.\n\nExample:\n    >>> apply_weights([0.0, 0.866, 1.0, 0.866, 0.0, -0.866, -1.0])\n    [4.33, 1.0, 5.196, 0.0, -4.33]"
  [table] (let [add (atom nil) i (atom nil)] (try (reset! add []) (doseq [__i1 (py-iter (range 1 (py-sub (count table) 1)))] (reset! i __i1) (if (py-truthy (let [__b2 (py-eq (py-mod @i 2) 0)] (if (py-truthy __b2) (not (py-eq (py-mod @i 3) 0)) __b2))) (do (swap! add conj (py-get table @i))) (do (if (py-truthy (let [__b3 (not (py-eq (py-mod @i 2) 0))] (if (py-truthy __b3) (not (py-eq (py-mod @i 3) 0)) __b3))) (do (swap! add conj (py-mul 5 (py-get table @i)))) (do (if (py-truthy (py-eq (py-mod @i 6) 0)) (do (swap! add conj (py-mul 2 (py-get table @i)))) (do (when (py-truthy (let [__b4 (py-eq (py-mod @i 3) 0)] (if (py-truthy __b4) (not (py-eq (py-mod @i 2) 0)) __b4))) (swap! add conj (py-mul 6 (py-get table @i))))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @add})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn compute-solution "Compute the final solution using the weighted values and table.\n\nArgs:\n    add (List[float]): A list of weighted values from apply_weights.\n    table (List[float]): A list of function values.\n    step_size (float): The step size calculated from the limits and accuracy.\n\nReturns:\n    float: The final computed integral solution.\n\nExample:\n    >>> compute_solution([4.33, 6.0, 0.0, -4.33], [0.0, 0.866, 1.0, 0.866, 0.0,\n    ... -0.866, -1.0], 0.5235983333333333)\n    0.7853975"
  [add table step-size] (py-mul (py-mul 0.3 step-size) (py-add (py-add (reduce +' 0 add) (py-get table 0)) (py-get table (py-neg 1)))))

;; NOTE: safe_function_eval stubbed (lambdify,symbols,sympify)
;; NOTE: compute_table stubbed (np)
