(ns clojure-algorithms.machine-learning.forecasting.run
  "this is code for forecasting\nbut I modified it and used it for safety checker of data\nfor ex: you have an online shop and for some reason some data are\nmissing (the amount of data that u expected are not supposed to be)\n        then we can use it\n*ps : 1. ofc we can use normal statistic method but in this case\n         the data is quite absurd and only a little^^\n      2. ofc u can use this and modified it for forecasting purpose\n         for the next 3 months sales or something,\n         u can just adjust it for ur own purpose"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare linear-regression-prediction sarimax-predictor support-vector-regressor random-forest-regressor interquartile-range-checker data-safety-checker)

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
