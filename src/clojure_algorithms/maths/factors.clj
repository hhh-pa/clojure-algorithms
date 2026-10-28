(ns clojure-algorithms.maths.factors
  "Port of maths/factors.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare factors-of-a-number)

(defn factors-of-a-number ">>> factors_of_a_number(1)\n[1]\n>>> factors_of_a_number(5)\n[1, 5]\n>>> factors_of_a_number(24)\n[1, 2, 3, 4, 6, 8, 12, 24]\n>>> factors_of_a_number(-24)\n[]"
  [num] (let [d (atom nil) facs (atom nil) i (atom nil)] (try (reset! facs []) (when (py-truthy (py-lt num 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @facs}))) (swap! facs conj 1) (when (py-truthy (py-eq num 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @facs}))) (swap! facs conj num) (doseq [__i1 (py-iter (range 2 (py-add (py-int (Math/sqrt num) nil) 1)))] (reset! i __i1) (when (py-truthy (py-eq (py-mod num @i) 0)) (swap! facs conj @i) (reset! d (py-floordiv num @i)) (when (py-truthy (not (py-eq @d @i))) (swap! facs conj @d)))) (reset! facs (py-sorted @facs)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @facs})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
