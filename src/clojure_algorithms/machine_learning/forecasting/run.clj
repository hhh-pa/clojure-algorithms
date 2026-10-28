(ns clojure-algorithms.machine-learning.forecasting.run
  "this is code for forecasting\nbut I modified it and used it for safety checker of data\nfor ex: you have an online shop and for some reason some data are\nmissing (the amount of data that u expected are not supposed to be)\n        then we can use it\n*ps : 1. ofc we can use normal statistic method but in this case\n         the data is quite absurd and only a little^^\n      2. ofc u can use this and modified it for forecasting purpose\n         for the next 3 months sales or something,\n         u can just adjust it for ur own purpose"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare linear-regression-prediction sarimax-predictor support-vector-regressor random-forest-regressor interquartile-range-checker data-safety-checker)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-sub py-abs py-float-str py-quote py-repr py-str)

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

(defn linear-regression-prediction "First method: linear regression\ninput : training data (date, total_user, total_event) in list of float\noutput : list of total user prediction in float\n>>> n = linear_regression_prediction([2,3,4,5], [5,3,4,6], [3,1,2,4], [2,1], [2,2])\n>>> bool(abs(n - 5.0) < 1e-6)  # Checking precision because of floating point errors\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot,numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot,numpy-attr:transpose" {:type :UnsupportedOperation})))

(defn sarimax-predictor "second method: Sarimax\nsarimax is a statistic method which using previous input\nand learn its pattern to predict future data\ninput : training data (total_user, with exog data = total_event) in list of float\noutput : list of total user prediction in float\n>>> sarimax_predictor([4,2,6,8], [3,1,2,4], [2])\n6.6666671111109626"
  [& _args]
  ;; stub: original requires SARIMAX; pure-Clojure port not applicable
  (throw (ex-info "requires SARIMAX" {:type :UnsupportedOperation})))

(defn support-vector-regressor "Third method: Support vector regressor\nsvr is quite the same with svm(support vector machine)\nit uses the same principles as the SVM for classification,\nwith only a few minor differences and the only different is that\nit suits better for regression purpose\ninput : training data (date, total_user, total_event) in list of float\nwhere x = list of set (date and total event)\noutput : list of total user prediction in float\n>>> support_vector_regressor([[5,2],[1,5],[6,2]], [[3,2]], [2,1,4])\n1.634932078116079"
  [& _args]
  ;; stub: original requires SVR; pure-Clojure port not applicable
  (throw (ex-info "requires SVR" {:type :UnsupportedOperation})))

(defn random-forest-regressor "Fourth method: Random Forest Regressor\nRandom Forest is an ensemble learning method for regression that operates\nby constructing a multitude of decision trees at training time and outputting\nthe mean prediction of the individual trees.\n\nIt is more robust than a single decision tree and less prone to overfitting.\nGood for capturing nonlinear relationships in data.\n\ninput : training data (date, total_event) in list of float\nwhere x = list of set (date and total event)\noutput : list of total user prediction in float\n\n>>> random_forest_regressor([[5,2],[1,5],[6,2]], [[3,2]], [2,1,4])\n1.95"
  [& _args]
  ;; stub: original requires RandomForestRegressor; pure-Clojure port not applicable
  (throw (ex-info "requires RandomForestRegressor" {:type :UnsupportedOperation})))

(defn interquartile-range-checker "Optional method: interquatile range\ninput : list of total user in float\noutput : low limit of input in float\nthis method can be used to check whether some data is outlier or not\n>>> interquartile_range_checker([1,2,3,4,5,6,7,8,9,10])\n2.8"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn data-safety-checker "Used to review all the votes (list result prediction)\nand compare it to the actual result.\ninput : list of predictions\noutput : print whether it's safe or not\n>>> data_safety_checker([2, 3, 4], 5.0)\nFalse"
  [list-vote actual-result] (let [i (atom nil) not-safe (atom nil) safe (atom nil)] (try (reset! safe 0) (reset! not-safe 0) (when (py-truthy (not (py-truthy (number? actual-result)))) (throw (ex-info (py-str "Actual result should be float. Value passed is a list") {:type :TypeError}))) (doseq [__i1 (py-iter list-vote)] (reset! i __i1) (if (py-truthy (py-gt @i actual-result)) (do (reset! safe (py-add @not-safe 1))) (do (if (py-truthy (py-le (py-abs (py-sub (py-abs @i) (py-abs actual-result))) 0.1)) (do (swap! safe py-add 1)) (do (swap! not-safe py-add 1)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-gt @safe @not-safe)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

;; NOTE: linear_regression_prediction stubbed (np,numpy-attr:dot,numpy-attr:transpose)
;; NOTE: sarimax_predictor stubbed (SARIMAX)
;; NOTE: support_vector_regressor stubbed (SVR)
;; NOTE: random_forest_regressor stubbed (RandomForestRegressor)
;; NOTE: interquartile_range_checker stubbed (np)
