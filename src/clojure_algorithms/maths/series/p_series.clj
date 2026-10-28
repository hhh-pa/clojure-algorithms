(ns clojure-algorithms.maths.series.p-series
  "This is a pure Python implementation of the P-Series algorithm
https://en.wikipedia.org/wiki/Harmonic_series_(mathematics)#P-series
For doctests run following command:
python -m doctest -v p_series.py
or
python3 -m doctest -v p_series.py
For manual testing run:
python3 p_series.py"
  (:require [clojure.string :as str]))

(defn- loose-int
  [v]
  (cond (integer? v) v
        (float? v) (int v)
        (string? v) (Long/parseLong (str/trim v))
        :else (int v)))

(defn- int-pow
  [base exp]
  (if (neg? exp)
    (Math/pow (double base) (double exp))
    (reduce *' 1 (repeat exp base))))

(defn p-series
  "Return the P-Series from 1 to the nth term as strings."
  [nth-term power]
  (if (= "" nth-term)
    [""]
    (let [n (loose-int nth-term)
          p (loose-int power)]
      (mapv (fn [i]
              (if (zero? i)
                "1"
                (str "1 / " (int-pow (inc i) p))))
            (range n)))))
