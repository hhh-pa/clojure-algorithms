(ns clojure-algorithms.maths.series.logarithmic-series
  "This is an implementation of logarithmic series in Python.
Reference: https://math.stackexchange.com/questions/3973429/what-is-a-logarithmic-series"
  (:require [clojure.string :as str]))

(defn logarithmic-series
  "Return the logarithmic series for x-value (log x) up to n-terms terms.

  With expand false (default) terms are unsolved expressions, with expand
  true they are real numbers."
  ([x-value] (logarithmic-series x-value 5 false))
  ([x-value n-terms] (logarithmic-series x-value n-terms false))
  ([x-value n-terms expand]
   (if expand
     (loop [n 1
            acc (double (- x-value 1))
            out []]
       (if (> n n-terms)
         out
         (recur (inc n)
                (* acc (- x-value 1))
                (conj out (* (if (odd? n) 1.0 -1.0) (/ acc n))))))
     (mapv (fn [n]
             (let [term (str (if (odd? n) "" "-")
                             "(" (- x-value 1) "^" n ")/" n)]
               (cond (str/starts-with? term "-(-") (str "(" (subs term 3))
                     (str/starts-with? term "(-") (str "-(" (subs term 2))
                     :else term)))
           (range 1 (inc n-terms))))))
