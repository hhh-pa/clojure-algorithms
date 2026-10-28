(ns clojure-algorithms.machine-learning.principle-component-analysis
  "Principal Component Analysis driver (STUB).

  Every entry point needs sklearn (Iris dataset, StandardScaler, PCA), which
  has no stdlib-only equivalent, so all functions are honest documented
  stubs throwing ex-info with :type :UnsupportedOperation.")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn collect-dataset
  "STUB: loads the Iris dataset via sklearn (needs load_iris)."
  [& _args]
  (unsupported! "load_iris"))

(defn apply-pca
  "STUB: standardizes and runs sklearn PCA (needs StandardScaler, PCA)."
  [& _args]
  (unsupported! "PCA,StandardScaler"))

(defn main
  "STUB: driver that collects the dataset and applies PCA."
  [& _args]
  (unsupported! "load_iris,PCA"))
