(ns clojure-algorithms.machine-learning.data-transformations
  "Normalization.\n\nWikipedia: https://en.wikipedia.org/wiki/Normalization\nNormalization is the process of converting numerical data to a standard range of values.\nThis range is typically between [0, 1] or [-1, 1]. The equation for normalization is\nx_norm = (x - x_min)/(x_max - x_min) where x_norm is the normalized value, x is the\nvalue, x_min is the minimum value within the column or list of data, and x_max is the\nmaximum value within the column or list of data. Normalization is used to speed up the\ntraining of data and put all of the data on a similar scale. This is useful because\nvariance in the range of values of a dataset can heavily impact optimization\n(particularly Gradient Descent).\n\nStandardization Wikipedia: https://en.wikipedia.org/wiki/Standardization\nStandardization is the process of converting numerical data to a normally distributed\nrange of values. This range will have a mean of 0 and standard deviation of 1. This is\nalso known as z-score normalization. The equation for standardization is\nx_std = (x - mu)/(sigma) where mu is the mean of the column or list of values and sigma\nis the standard deviation of the column or list of values.\n\nChoosing between Normalization & Standardization is more of an art of a science, but it\nis often recommended to run experiments with both to see which performs better.\nAdditionally, a few rules of thumb are:\n    1. gaussian (normal) distributions work better with standardization\n    2. non-gaussian (non-normal) distributions work better with normalization\n    3. If a column or list of values has extreme values / outliers, use standardization"
  (:require [clojure.set :as set]))

(declare normalization standardization)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-gt py-iter py-sub py-div py-min py-min-by py-max py-max-by py-round py-stat-mean py-stat-variance py-stat-stdev)

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

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

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

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-round
  ([x] (long (Math/rint (double x))))
  ([x n]
   (if (nil? n) (py-round x)
       (let [nn (long n)]
         (if (>= nn 0)
           (let [f (Math/pow 10.0 (double nn))]
             (/ (Math/rint (* (double x) f)) f))
           (let [f (Math/pow 10.0 (double (- nn)))]
             (* (Math/rint (/ (double x) f)) f)))))))

(defn- py-stat-mean [c]
  (let [v (vec c)]
    (when (empty? v) (throw (ex-info "mean requires at least one data point" {:type :ValueError})))
    (let [r (/ (reduce +' 0 v) (count v))]
      (if (ratio? r) (double r) r))))

(defn- py-stat-variance [c]
  (let [v (vec c)]
    (when (< (count v) 2) (throw (ex-info "variance requires at least two data points" {:type :ValueError})))
    (let [m (/ (reduce +' 0 v) (count v))
          r (/ (reduce +' 0 (map (fn [x] (let [d (-' x m)] (*' d d))) v)) (dec (count v)))]
      (if (ratio? r) (double r) r))))

(defn- py-stat-stdev [c] (Math/sqrt (double (py-stat-variance c))))

(defn normalization "Return a normalized list of values.\n\n@params: data, a list of values to normalize\n@returns: a list of normalized values (rounded to ndigits decimal places)\n@examples:\n>>> normalization([2, 7, 10, 20, 30, 50])\n[0.0, 0.104, 0.167, 0.375, 0.583, 1.0]\n>>> normalization([5, 10, 15, 20, 25])\n[0.0, 0.25, 0.5, 0.75, 1.0]"
  ([data] (normalization data 3))
  ([data ndigits] (let [x-max (atom nil) x-min (atom nil)] (try (reset! x-min (py-min data)) (reset! x-max (py-max data)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [x (py-iter data)] (py-round (py-div (py-sub x @x-min) (py-sub @x-max @x-min)) ndigits)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn standardization "Return a standardized list of values.\n\n@params: data, a list of values to standardize\n@returns: a list of standardized values (rounded to ndigits decimal places)\n@examples:\n>>> standardization([2, 7, 10, 20, 30, 50])\n[-0.999, -0.719, -0.551, 0.009, 0.57, 1.69]\n>>> standardization([5, 10, 15, 20, 25])\n[-1.265, -0.632, 0.0, 0.632, 1.265]"
  ([data] (standardization data 3))
  ([data ndigits] (let [mu (atom nil) sigma (atom nil)] (try (reset! mu (py-stat-mean data)) (reset! sigma (py-stat-stdev data)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [x (py-iter data)] (py-round (py-div (py-sub x @mu) @sigma) ndigits)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
