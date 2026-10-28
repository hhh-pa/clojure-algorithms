(ns clojure-algorithms.machine-learning.linear-regression
  "Linear regression is the most basic type of regression commonly used for\npredictive analysis. The idea is pretty simple: we have a dataset, and we have\nfeatures associated with it. Features should be chosen very cautiously\nas they determine how much our model will be able to make future predictions.\nWe try to set the weight of these features, over many iterations, so that they best\nfit our dataset. In this particular code, I used a CSGO dataset (ADR vs\nRating). We try to best fit a line through the dataset and estimate the parameters."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare collect-dataset run-steep-gradient-descent sum-of-square-error run-linear-regression mean-absolute-error plot-regression plot-loss main)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-iter py-index-norm py-get py-sub py-div py-abs py-float-str py-quote py-repr)

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

(defn collect-dataset "Collect dataset of CSGO\nThe dataset contains ADR vs Rating of a Player\n:return : dataset obtained from the link, as a matrix"
  [& _args]
  ;; stub: original requires httpx2,np; pure-Clojure port not applicable
  (throw (ex-info "requires httpx2,np" {:type :UnsupportedOperation})))

(defn run-steep-gradient-descent "Run steep gradient descent and update the Feature vector accordingly_\n:param data_x   : contains the dataset\n:param data_y   : contains the output associated with each data-entry\n:param len_data : length of the data_\n:param alpha    : Learning rate of the model\n:param theta    : Feature vector (weights for our model)\n;param return    : Updated features, using\n                   curr_features - alpha_ * gradient(w.r.t. feature)\n>>> import numpy as np\n>>> data_x = np.array([[1, 2], [3, 4]])\n>>> data_y = np.array([5, 6])\n>>> len_data = len(data_x)\n>>> alpha = 0.01\n>>> theta = np.array([0.1, 0.2])\n>>> run_steep_gradient_descent(data_x, data_y, len_data, alpha, theta)\narray([0.196, 0.343])"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot,numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot,numpy-attr:transpose" {:type :UnsupportedOperation})))

(defn sum-of-square-error "Return sum of square error for error calculation\n:param data_x    : contains our dataset\n:param data_y    : contains the output (result vector)\n:param theta     : contains the feature vector\n:return          : sum of square error computed from given feature's\n\nExample:\n>>> vc_x = np.array([[1.1], [2.1], [3.1]])\n>>> vc_y = np.array([1.2, 2.2, 3.2])\n>>> round(sum_of_square_error(vc_x, vc_y, np.array([1])), 3)\nnp.float64(0.03)"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot,numpy-attr:sum,numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot,numpy-attr:sum,numpy-attr:transpose" {:type :UnsupportedOperation})))

(defn run-linear-regression "Implement Linear regression over the dataset\n:param data_x  : contains our dataset\n:param data_y  : contains the output (result vector)\n:return        : feature for line of best fit (Feature vector)"
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn mean-absolute-error "Return sum of square error for error calculation\n:param predicted_y   : contains the output of prediction (result vector)\n:param original_y    : contains values of expected outcome\n:return          : mean absolute error computed from given features\n\n>>> predicted_y = [3, -0.5, 2, 7]\n>>> original_y = [2.5, 0.0, 2, 8]\n>>> mean_absolute_error(predicted_y, original_y)\n0.5"
  [predicted-y original-y] (let [total (atom nil)] (try (reset! total (reduce +' 0 (doall (for [[i y] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) original-y))] (py-abs (py-sub y (py-get predicted-y i))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div @total (count original-y))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn plot-regression "Plot regression line with dataset points"
  [& _args]
  ;; stub: original requires np,numpy-attr:flatten,plt; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:flatten,plt" {:type :UnsupportedOperation})))

(defn plot-loss "Plot training loss curve"
  [& _args]
  ;; stub: original requires plt; pure-Clojure port not applicable
  (throw (ex-info "requires plt" {:type :UnsupportedOperation})))

(defn main "Driver function"
  [& _args]
  ;; stub: original requires np,numpy-attr:astype,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:astype,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: collect_dataset stubbed (httpx2,np)
;; NOTE: run_steep_gradient_descent stubbed (np,numpy-attr:dot,numpy-attr:transpose)
;; NOTE: sum_of_square_error stubbed (np,numpy-attr:dot,numpy-attr:sum,numpy-attr:transpose)
;; NOTE: run_linear_regression stubbed (np,numpy-attr:shape)
;; NOTE: plot_regression stubbed (np,numpy-attr:flatten,plt)
;; NOTE: plot_loss stubbed (plt)
;; NOTE: main stubbed (np,numpy-attr:astype,numpy-attr:shape)
