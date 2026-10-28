(ns clojure-algorithms.maths.series.harmonic-series
  "This is a pure Python implementation of the Harmonic Series algorithm\nhttps://en.wikipedia.org/wiki/Harmonic_series_(mathematics)\n\nFor doctests run following command:\npython -m doctest -v harmonic_series.py\nor\npython3 -m doctest -v harmonic_series.py\n\nFor manual testing run:\npython3 harmonic_series.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare harmonic-series)

(defn harmonic-series "Pure Python implementation of Harmonic Series algorithm\n\n:param n_term: The last (nth) term of Harmonic Series\n:return: The Harmonic Series starting from 1 to last (nth) term\n\nExamples:\n>>> harmonic_series(5)\n['1', '1/2', '1/3', '1/4', '1/5']\n>>> harmonic_series(5.0)\n['1', '1/2', '1/3', '1/4', '1/5']\n>>> harmonic_series(5.1)\n['1', '1/2', '1/3', '1/4', '1/5']\n>>> harmonic_series(-5)\n[]\n>>> harmonic_series(0)\n[]\n>>> harmonic_series(1)\n['1']"
  [n-term] (let [series (atom nil) temp (atom nil)] (try (when (py-truthy (py-eq n-term "")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (reset! series []) (doseq [__i1 (py-iter (range (py-int n-term nil)))] (reset! temp __i1) (swap! series conj (if (py-truthy @series) (str "1/" (py-str (py-add @temp 1))) "1"))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @series})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
