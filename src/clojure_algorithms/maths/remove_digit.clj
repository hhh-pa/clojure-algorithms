(ns clojure-algorithms.maths.remove-digit
  "Port of maths/remove_digit.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare remove-digit)

(defn remove-digit "returns the biggest possible result\nthat can be achieved by removing\none digit from the given number\n\n>>> remove_digit(152)\n52\n>>> remove_digit(6385)\n685\n>>> remove_digit(-11)\n1\n>>> remove_digit(2222222)\n222222\n>>> remove_digit(\"2222222\")\nTraceback (most recent call last):\nTypeError: only integers accepted as input\n>>> remove_digit(\"string input\")\nTraceback (most recent call last):\nTypeError: only integers accepted as input"
  [num] (let [index (atom nil) num-str (atom nil) num-transpositions (atom nil)] (try (when (py-truthy (not (py-truthy (integer? num)))) (throw (ex-info (py-str "only integers accepted as input") {:type :TypeError}))) (reset! num-str (py-str (py-abs num))) (reset! num-transpositions (vec (for [char (py-iter (range (count @num-str)))] (py-list @num-str)))) (doseq [__i1 (py-iter (range (count @num-str)))] (reset! index __i1) (py-pop (py-get @num-transpositions @index) @index)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-max (doall (for [transposition (py-iter @num-transpositions)] (py-int (clojure.string/join "" (map py-str (py-list transposition))) nil))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
