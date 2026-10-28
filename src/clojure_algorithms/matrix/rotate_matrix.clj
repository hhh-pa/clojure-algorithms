(ns clojure-algorithms.matrix.rotate-matrix
  "In this problem, we want to rotate the matrix elements by 90, 180, 270\n(counterclockwise)\nDiscussion in stackoverflow:\nhttps://stackoverflow.com/questions/42519/how-do-you-rotate-a-two-dimensional-array"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare make-matrix rotate-90 rotate-180 rotate-270 transpose reverse-row reverse-column print-matrix)

(defn make-matrix ">>> make_matrix()\n[[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]\n>>> make_matrix(1)\n[[1]]\n>>> make_matrix(-2)\n[[1, 2], [3, 4]]\n>>> make_matrix(3)\n[[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n>>> make_matrix() == make_matrix(4)\nTrue"
  ([] (make-matrix 4))
  ([row-size] (let [row-size (atom row-size)] (try (reset! row-size (let [__b1 (py-abs @row-size)] (if (py-truthy __b1) __b1 4))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [y (py-iter (range @row-size))] (vec (for [x (py-iter (range @row-size))] (py-add (py-add 1 x) (py-mul y @row-size))))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn rotate-90 ">>> rotate_90(make_matrix())\n[[4, 8, 12, 16], [3, 7, 11, 15], [2, 6, 10, 14], [1, 5, 9, 13]]\n>>> rotate_90(make_matrix()) == transpose(reverse_column(make_matrix()))\nTrue"
  [matrix] (reverse-row (transpose matrix)))

(defn rotate-180 ">>> rotate_180(make_matrix())\n[[16, 15, 14, 13], [12, 11, 10, 9], [8, 7, 6, 5], [4, 3, 2, 1]]\n>>> rotate_180(make_matrix()) == reverse_column(reverse_row(make_matrix()))\nTrue"
  [matrix] (reverse-row (reverse-column matrix)))

(defn rotate-270 ">>> rotate_270(make_matrix())\n[[13, 9, 5, 1], [14, 10, 6, 2], [15, 11, 7, 3], [16, 12, 8, 4]]\n>>> rotate_270(make_matrix()) == transpose(reverse_row(make_matrix()))\nTrue"
  [matrix] (reverse-column (transpose matrix)))

(defn transpose
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (vec (for [x (py-iter (apply map vector @matrix))] (py-list x)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn reverse-row
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (py-slice @matrix nil nil (py-neg 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn reverse-column
  [matrix] (let [matrix (atom matrix)] (try (swap! matrix (fn [__c] (py-assoc-in __c [[:slice nil nil nil]] (vec (for [x (py-iter @matrix)] (py-slice x nil nil (py-neg 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @matrix})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn print-matrix
  [matrix] (let [i (atom nil)] (doseq [__i1 (py-iter matrix)] (reset! i __i1) (print (str (clojure.string/join " " (concat (map py-str (seq @i)))) "\n"))) nil))
