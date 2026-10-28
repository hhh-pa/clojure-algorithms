(ns clojure-algorithms.machine-learning.random-forest-regressor
  "Port of machine_learning/random_forest_regressor.py")

(declare main)

(defn main "Random Forest Regressor Example using sklearn function.\nThe diabetes dataset is used to demonstrate the algorithm.\n\nNote: this example previously used the Boston house-price dataset,\nwhich was removed from scikit-learn (>=1.2) for ethical reasons.\n``load_diabetes`` is a drop-in bundled alternative that ships with\nscikit-learn, so the example runs offline."
  [& _args]
  ;; stub: original requires RandomForestRegressor,load_diabetes,mean_absolute_error,mean_squared_error,numpy-attr:reshape,train_test_split; pure-Clojure port not applicable
  (throw (ex-info "requires RandomForestRegressor,load_diabetes,mean_absolute_error,mean_squared_error,numpy-attr:reshape,train_test_split" {:type :UnsupportedOperation})))

;; NOTE: main stubbed (RandomForestRegressor,load_diabetes,mean_absolute_error,mean_squared_error,numpy-attr:reshape,train_test_split)
