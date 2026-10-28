(ns clojure-algorithms.maths.abs
  "Absolute Value."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare abs-val abs-min abs-max abs-max-sort test-abs-val)

(defn abs-val "Find the absolute value of a number.\n\n>>> abs_val(-5.1)\n5.1\n>>> abs_val(-5) == abs_val(5)\nTrue\n>>> abs_val(0)\n0"
  [num] (if (py-truthy (py-lt num 0)) (py-neg num) num))

(defn abs-min ">>> abs_min([0,5,1,11])\n0\n>>> abs_min([3,-10,-2])\n-2\n>>> abs_min([])\nTraceback (most recent call last):\n    ...\nValueError: abs_min() arg is an empty sequence"
  [x] (let [i (atom nil) j (atom nil)] (try (when (py-truthy (py-eq (count x) 0)) (throw (ex-info (py-str "abs_min() arg is an empty sequence") {:type :ValueError}))) (reset! j (py-get x 0)) (doseq [__i1 (py-iter x)] (reset! i __i1) (when (py-truthy (py-lt (abs-val @i) (abs-val @j))) (reset! j @i))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @j})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn abs-max ">>> abs_max([0,5,1,11])\n11\n>>> abs_max([3,-10,-2])\n-10\n>>> abs_max([])\nTraceback (most recent call last):\n    ...\nValueError: abs_max() arg is an empty sequence"
  [x] (let [i (atom nil) j (atom nil)] (try (when (py-truthy (py-eq (count x) 0)) (throw (ex-info (py-str "abs_max() arg is an empty sequence") {:type :ValueError}))) (reset! j (py-get x 0)) (doseq [__i1 (py-iter x)] (reset! i __i1) (when (py-truthy (py-gt (py-abs @i) (py-abs @j))) (reset! j @i))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @j})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn abs-max-sort ">>> abs_max_sort([0,5,1,11])\n11\n>>> abs_max_sort([3,-10,-2])\n-10\n>>> abs_max_sort([])\nTraceback (most recent call last):\n    ...\nValueError: abs_max_sort() arg is an empty sequence"
  [x] (try (when (py-truthy (py-eq (count x) 0)) (throw (ex-info (py-str "abs_max_sort() arg is an empty sequence") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (vec (sort-by py-abs x)) (py-neg 1))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn test-abs-val ">>> test_abs_val()"
  [] (let [a (atom nil)] (assert (py-truthy (py-eq (abs-val 0) 0))) (assert (py-truthy (py-eq (abs-val 34) 34))) (assert (py-truthy (py-eq (abs-val (py-neg 100000000000)) 100000000000))) (reset! a [(py-neg 3) (py-neg 1) 2 (py-neg 11)]) (assert (py-truthy (py-eq (abs-max @a) (py-neg 11)))) (assert (py-truthy (py-eq (abs-max-sort @a) (py-neg 11)))) (assert (py-truthy (py-eq (abs-min @a) (py-neg 1)))) nil))
