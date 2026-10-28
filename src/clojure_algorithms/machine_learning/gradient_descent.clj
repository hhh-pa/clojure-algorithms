(ns clojure-algorithms.machine-learning.gradient-descent
  "Implementation of gradient descent algorithm for minimizing cost of a linear hypothesis\nfunction."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare -error -hypothesis-value output calculate-hypothesis-value summation-of-cost-derivative get-cost-derivative run-gradient-descent test-gradient-descent train-data test-data parameter-vector m LEARNING-RATE)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-repeat-vec py-mul py-div py-neg py-float-str py-quote py-repr py-str)

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

(defn -error ":param data_set: train data or test data\n:param example_no: example number whose error has to be checked\n:return: error in example pointed by example number.\n\n>>> _error(0)  # hypothesis 39 minus train output 15\n24"
  ([example-no] (-error example-no "train"))
  ([example-no data-set] (py-sub (calculate-hypothesis-value example-no data-set) (output example-no data-set))))

(defn -hypothesis-value "Calculates hypothesis function value for a given input\n:param data_input_tuple: Input tuple of a particular example\n:return: Value of hypothesis function at that point.\nNote that there is an 'biased input' whose value is fixed as 1.\nIt is not explicitly mentioned in input data.. But, ML hypothesis functions use it.\nSo, we have to take care of it separately. Line 36 takes care of it.\n\n>>> _hypothesis_value((5, 2, 3))  # 5*4 + 2*1 + 3*5 + bias 2\n39"
  [data-input-tuple] (let [hyp-val (atom nil) i (atom nil)] (try (reset! hyp-val 0) (doseq [__i1 (py-iter (range (py-sub (count @parameter-vector) 1)))] (reset! i __i1) (swap! hyp-val py-add (py-mul (py-get data-input-tuple @i) (py-get @parameter-vector (py-add @i 1))))) (swap! hyp-val py-add (py-get @parameter-vector 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @hyp-val})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn output ":param data_set: test data or train data\n:param example_no: example whose output is to be fetched\n:return: output for that example\n\n>>> output(0, \"train\")\n15\n>>> output(1, \"test\")\n150\n>>> output(0, \"unknown\") is None\nTrue"
  [example-no data-set] (try (if (py-truthy (py-eq data-set "train")) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get train-data example-no) 1)}))) (do (when (py-truthy (py-eq data-set "test")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get test-data example-no) 1)}))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn calculate-hypothesis-value "Calculates hypothesis value for a given example\n:param data_set: test data or train_data\n:param example_no: example whose hypothesis value is to be calculated\n:return: hypothesis value for that example\n\n>>> calculate_hypothesis_value(0, \"train\")\n39\n>>> calculate_hypothesis_value(0, \"test\")\n2149\n>>> calculate_hypothesis_value(0, \"unknown\") is None\nTrue"
  [example-no data-set] (try (if (py-truthy (py-eq data-set "train")) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (-hypothesis-value (py-get (py-get train-data example-no) 0))}))) (do (when (py-truthy (py-eq data-set "test")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (-hypothesis-value (py-get (py-get test-data example-no) 0))}))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn summation-of-cost-derivative "Calculates the sum of cost function derivative\n:param index: index wrt derivative is being calculated\n:param end: value where summation ends, default is m, number of examples\n:return: Returns the summation of cost derivative\nNote: If index is -1, this means we are calculating summation wrt to biased\n    parameter.\n\n>>> summation_of_cost_derivative(-1)  # sum of errors over all examples\n243\n>>> summation_of_cost_derivative(0)\n2234"
  ([index] (summation-of-cost-derivative index m))
  ([index end] (let [i (atom nil) summation-value (atom nil)] (try (reset! summation-value 0) (doseq [__i1 (py-iter (range end))] (reset! i __i1) (if (py-truthy (py-eq index (py-neg 1))) (do (swap! summation-value py-add (-error @i))) (do (swap! summation-value py-add (py-mul (-error @i) (py-get (py-get (py-get train-data @i) 0) index)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @summation-value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn get-cost-derivative ":param index: index of the parameter vector wrt to derivative is to be calculated\n:return: derivative wrt to that index\nNote: If index is -1, this means we are calculating summation wrt to biased\n    parameter.\n\n>>> get_cost_derivative(-1)  # 243 / 5 examples\n48.6\n>>> get_cost_derivative(0)  # 2234 / 5 examples\n446.8"
  [index] (let [cost-derivative-value (atom nil)] (try (reset! cost-derivative-value (py-div (summation-of-cost-derivative index m) m)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @cost-derivative-value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn run-gradient-descent
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn test-gradient-descent
  [] (let [i (atom nil)] (doseq [__i1 (py-iter (range (count test-data)))] (reset! i __i1) (println (clojure.string/join " " [(py-str ["Actual output value:" (output @i "test")])])) (println (clojure.string/join " " [(py-str ["Hypothesis output:" (calculate-hypothesis-value @i "test")])]))) nil))

(def train-data [[[5 2 3] 15] [[6 5 9] 25] [[11 12 13] 41] [[1 1 1] 8] [[11 12 13] 41]])

(def test-data [[[515 22 13] 555] [[61 35 49] 150]])

(def parameter-vector (atom [2 4 1 5]))

(def m (count train-data))

(def LEARNING-RATE 0.009)

;; NOTE: run_gradient_descent stubbed (np)
