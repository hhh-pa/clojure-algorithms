(ns clojure-algorithms.machine-learning.decision-tree
  "Implementation of a basic regression decision tree.\nInput data set: The input data set must be 1-dimensional with continuous labels.\nOutput: The decision tree maps a real number input to a real number output."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare main make-DecisionTree DecisionTree-mean-squared-error DecisionTree-train DecisionTree-predict make-TestDecisionTree TestDecisionTree-helper-mean-squared-error-test)

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
