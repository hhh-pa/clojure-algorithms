(ns clojure-algorithms.maths.series.p-series
  "This is a pure Python implementation of the P-Series algorithm\nhttps://en.wikipedia.org/wiki/Harmonic_series_(mathematics)#P-series\nFor doctests run following command:\npython -m doctest -v p_series.py\nor\npython3 -m doctest -v p_series.py\nFor manual testing run:\npython3 p_series.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare p-series)

(defn p-series "Pure Python implementation of P-Series algorithm\n:return: The P-Series starting from 1 to last (nth) term\nExamples:\n>>> p_series(5, 2)\n['1', '1 / 4', '1 / 9', '1 / 16', '1 / 25']\n>>> p_series(-5, 2)\n[]\n>>> p_series(5, -2)\n['1', '1 / 0.25', '1 / 0.1111111111111111', '1 / 0.0625', '1 / 0.04']\n>>> p_series(\"\", 1000)\n['']\n>>> p_series(0, 0)\n[]\n>>> p_series(1, 1)\n['1']"
  [nth-term power] (let [nth-term (atom nth-term) power (atom power)] (let [series (atom nil) temp (atom nil)] (try (when (py-truthy (py-eq @nth-term "")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [""]}))) (reset! nth-term (py-int @nth-term nil)) (reset! power (py-int @power nil)) (reset! series []) (doseq [__i1 (py-iter (range (py-int @nth-term nil)))] (reset! temp __i1) (swap! series conj (if (py-truthy @series) (str "1 / " (py-str (py-pow (py-add @temp 1) (py-int @power nil)))) "1"))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @series})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
