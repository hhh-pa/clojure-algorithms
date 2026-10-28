(ns clojure-algorithms.machine-learning.scoring-functions
  "Port of machine_learning/scoring_functions.py")

(declare mae mse rmse rmsle mbd manual-accuracy)

(defn mae "Examples(rounded for precision):\n>>> actual = [1,2,3];predict = [1,4,3]\n>>> float(np.around(mae(predict,actual),decimals = 2))\n0.67\n\n>>> actual = [1,1,1];predict = [1,1,1]\n>>> float(mae(predict,actual))\n0.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn mse "Examples(rounded for precision):\n>>> actual = [1,2,3];predict = [1,4,3]\n>>> float(np.around(mse(predict,actual),decimals = 2))\n1.33\n\n>>> actual = [1,1,1];predict = [1,1,1]\n>>> float(mse(predict,actual))\n0.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn rmse "Examples(rounded for precision):\n>>> actual = [1,2,3];predict = [1,4,3]\n>>> float(np.around(rmse(predict,actual),decimals = 2))\n1.15\n\n>>> actual = [1,1,1];predict = [1,1,1]\n>>> float(rmse(predict,actual))\n0.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn rmsle "Examples(rounded for precision):\n>>> float(np.around(rmsle(predict=[10, 2, 30], actual=[10, 10, 30]), decimals=2))\n0.75\n\n>>> float(rmsle(predict=[1, 1, 1], actual=[1, 1, 1]))\n0.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn mbd "This value is Negative, if the model underpredicts,\npositive, if it overpredicts.\n\nExample(rounded for precision):\n\nHere the model overpredicts\n>>> actual = [1,2,3];predict = [2,3,4]\n>>> float(np.around(mbd(predict,actual),decimals = 2))\n50.0\n\nHere the model underpredicts\n>>> actual = [1,2,3];predict = [0,1,1]\n>>> float(np.around(mbd(predict,actual),decimals = 2))\n-66.67"
  [& _args]
  ;; stub: original requires np,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn manual-accuracy
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

;; NOTE: mae stubbed (np,numpy-attr:mean)
;; NOTE: mse stubbed (np,numpy-attr:mean)
;; NOTE: rmse stubbed (np,numpy-attr:mean)
;; NOTE: rmsle stubbed (np,numpy-attr:mean)
;; NOTE: mbd stubbed (np,numpy-attr:sum)
;; NOTE: manual_accuracy stubbed (np,numpy-attr:mean)
