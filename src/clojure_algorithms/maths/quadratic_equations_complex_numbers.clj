(ns clojure-algorithms.maths.quadratic-equations-complex-numbers
  "Port of maths/quadratic_equations_complex_numbers.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare quadratic-roots main)

(defn quadratic-roots "Given the numerical coefficients a, b and c,\ncalculates the roots for any quadratic equation of the form ax^2 + bx + c\n\n>>> quadratic_roots(a=1, b=3, c=-4)\n(1.0, -4.0)\n>>> quadratic_roots(5, 6, 1)\n(-0.2, -1.0)\n>>> quadratic_roots(1, -6, 25)\n((3+4j), (3-4j))"
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn main
  [] (let [solution1 (atom nil) solution2 (atom nil)] (let [__d1 (vec (quadratic-roots nil nil nil))] (reset! solution1 (nth __d1 0)) (reset! solution2 (nth __d1 1))) (println (clojure.string/join " " [(py-str (str "The solutions are: " (py-str @solution1) " and " (py-str @solution2)))])) nil))

;; NOTE: quadratic_roots stubbed (complex-attr)
