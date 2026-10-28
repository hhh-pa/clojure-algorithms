(ns clojure-algorithms.machine-learning.decision-tree
  "Implementation of a basic regression decision tree.\nInput data set: The input data set must be 1-dimensional with continuous labels.\nOutput: The decision tree maps a real number input to a real number output."
  (:require [clojure.string :as str]))

(declare main make-DecisionTree DecisionTree-mean-squared-error DecisionTree-train DecisionTree-predict make-TestDecisionTree TestDecisionTree-helper-mean-squared-error-test)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-float-str py-quote py-repr py-str)

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

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

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

(defrecord DecisionTree [depth decision-boundary left right min-leaf-size prediction])

(defrecord TestDecisionTree [])

(defn make-DecisionTree "Constructor for DecisionTree."
  ([] (make-DecisionTree 5 5))
  ([depth] (make-DecisionTree depth 5))
  ([depth min-leaf-size] (let [self (atom {})] (try (swap! self (fn [__c] (py-assoc-in __c [:depth] depth))) (swap! self (fn [__c] (py-assoc-in __c [:decision-boundary] 0))) (swap! self (fn [__c] (py-assoc-in __c [:left] nil))) (swap! self (fn [__c] (py-assoc-in __c [:right] nil))) (swap! self (fn [__c] (py-assoc-in __c [:min-leaf-size] min-leaf-size))) (swap! self (fn [__c] (py-assoc-in __c [:prediction] nil))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->DecisionTree @self))))

(defn make-TestDecisionTree
  "Constructor for TestDecisionTree."
  ([] (map->TestDecisionTree {})))

(defn DecisionTree-mean-squared-error "mean_squared_error:\n@param labels: a one-dimensional numpy array\n@param prediction: a floating point value\nreturn value: mean_squared_error calculates the error if prediction is used to\n    estimate the labels\n>>> tester = DecisionTree()\n>>> test_labels = np.array([1,2,3,4,5,6,7,8,9,10])\n>>> test_prediction = float(6)\n>>> bool(tester.mean_squared_error(test_labels, test_prediction) == (\n...     TestDecisionTree.helper_mean_squared_error_test(test_labels,\n...         test_prediction)))\nTrue\n>>> test_labels = np.array([1,2,3])\n>>> test_prediction = float(2)\n>>> bool(tester.mean_squared_error(test_labels, test_prediction) == (\n...     TestDecisionTree.helper_mean_squared_error_test(test_labels,\n...         test_prediction)))\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean,numpy-attr:ndim; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean,numpy-attr:ndim" {:type :UnsupportedOperation})))

(defn DecisionTree-train "train:\n@param x: a one-dimensional numpy array\n@param y: a one-dimensional numpy array.\nThe contents of y are the labels for the corresponding X values\n\ntrain() does not have a return value\n\nExamples:\n1. Try to train when x & y are of same length & 1 dimensions (No errors)\n>>> dt = DecisionTree()\n>>> dt.train(np.array([10,20,30,40,50]),np.array([0,0,0,1,1]))\n\n2. Try to train when x is 2 dimensions\n>>> dt = DecisionTree()\n>>> dt.train(np.array([[1,2,3,4,5],[1,2,3,4,5]]),np.array([0,0,0,1,1]))\nTraceback (most recent call last):\n    ...\nValueError: Input data set must be one-dimensional\n\n3. Try to train when x and y are not of the same length\n>>> dt = DecisionTree()\n>>> dt.train(np.array([1,2,3,4,5]),np.array([[0,0,0,1,1],[0,0,0,1,1]]))\nTraceback (most recent call last):\n    ...\nValueError: x and y have different lengths\n\n4. Try to train when x & y are of the same length but different dimensions\n>>> dt = DecisionTree()\n>>> dt.train(np.array([1,2,3,4,5]),np.array([[1],[2],[3],[4],[5]]))\nTraceback (most recent call last):\n    ...\nValueError: Data set labels must be one-dimensional\n\nThis section is to check that the inputs conform to our dimensionality\nconstraints"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean,numpy-attr:ndim; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean,numpy-attr:ndim" {:type :UnsupportedOperation})))

(defn DecisionTree-predict "predict:\n@param x: a floating point value to predict the label of\nthe prediction function works by recursively calling the predict function\nof the appropriate subtrees based on the tree's decision boundary"
  [self x] (try (if (py-truthy (some? (:prediction @self))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (:prediction @self)}))) (do (if (py-truthy (let [__b1 (some? (:left @self))] (if (py-truthy __b1) (some? (:right @self)) __b1))) (do (if (py-truthy (py-ge x (:decision-boundary @self))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (DecisionTree-predict (atom (:right @self)) x)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (DecisionTree-predict (atom (:left @self)) x)}))))) (do (throw (ex-info (py-str "Decision tree not yet trained") {:type :ValueError})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn TestDecisionTree-helper-mean-squared-error-test "helper_mean_squared_error_test:\n@param labels: a one dimensional numpy array\n@param prediction: a floating point value\nreturn value: helper_mean_squared_error_test calculates the mean squared error"
  [& _args]
  ;; stub: original requires numpy-attr:size; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:size" {:type :UnsupportedOperation})))

(defn main "In this demonstration we're generating a sample data set from the sin function in\nnumpy.  We then train a decision tree on the data set and use the decision tree to\npredict the label of 10 different test values. Then the mean squared error over\nthis test is displayed."
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

;; NOTE: DecisionTree.mean_squared_error stubbed (np,numpy-attr:mean,numpy-attr:ndim)
;; NOTE: DecisionTree.train stubbed (np,numpy-attr:mean,numpy-attr:ndim)
;; NOTE: TestDecisionTree.helper_mean_squared_error_test stubbed (numpy-attr:size)
;; NOTE: main stubbed (np,numpy-attr:mean)
