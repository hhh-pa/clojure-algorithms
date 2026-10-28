(ns clojure-algorithms.matrix.matrix-multiplication-recursion
  "Perform matrix multiplication using a recursive algorithm.\nhttps://en.wikipedia.org/wiki/Matrix_multiplication"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare is-square matrix-multiply matrix-multiply-recursive Matrix matrix-1-to-4 matrix-5-to-8 matrix-5-to-9-high matrix-5-to-9-wide matrix-count-up matrix-unordered matrices)

(defn is-square ">>> is_square([])\nTrue\n>>> is_square(matrix_1_to_4)\nTrue\n>>> is_square(matrix_5_to_9_high)\nFalse"
  [matrix] (let [len-matrix (atom nil)] (try (reset! len-matrix (count matrix)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (every? py-truthy (doall (for [row (py-iter matrix)] (py-eq (count row) @len-matrix))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn matrix-multiply ">>> matrix_multiply(matrix_1_to_4, matrix_5_to_8)\n[[19, 22], [43, 50]]"
  [matrix-a matrix-b] (vec (for [row (py-iter matrix-a)] (vec (for [col (py-iter (apply map vector matrix-b))] (reduce +' 0 (doall (for [[a b] (py-iter (map vector row col))] (py-mul a b)))))))))

(defn matrix-multiply-recursive ":param matrix_a: A square Matrix.\n:param matrix_b: Another square Matrix with the same dimensions as matrix_a.\n:return: Result of matrix_a * matrix_b.\n:raises ValueError: If the matrices cannot be multiplied.\n\n>>> matrix_multiply_recursive([], [])\n[]\n>>> matrix_multiply_recursive(matrix_1_to_4, matrix_5_to_8)\n[[19, 22], [43, 50]]\n>>> matrix_multiply_recursive(matrix_count_up, matrix_unordered)\n[[37, 61, 74, 61], [105, 165, 166, 129], [173, 269, 258, 197], [241, 373, 350, 265]]\n>>> matrix_multiply_recursive(matrix_1_to_4, matrix_5_to_9_wide)\nTraceback (most recent call last):\n    ...\nValueError: Invalid matrix dimensions\n>>> matrix_multiply_recursive(matrix_1_to_4, matrix_5_to_9_high)\nTraceback (most recent call last):\n    ...\nValueError: Invalid matrix dimensions\n>>> matrix_multiply_recursive(matrix_1_to_4, matrix_count_up)\nTraceback (most recent call last):\n    ...\nValueError: Invalid matrix dimensions"
  [matrix-a matrix-b] (let [result (atom nil)] (try (when (py-truthy (let [__b1 (not (py-truthy matrix-a))] (if (py-truthy __b1) __b1 (not (py-truthy matrix-b))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value []}))) (when (py-truthy (not (py-truthy (every? py-truthy [(py-eq (count matrix-a) (count matrix-b)) (is-square matrix-a) (is-square matrix-b)])))) (throw (ex-info (py-str "Invalid matrix dimensions") {:type :ValueError}))) (reset! result (vec (for [--v (py-iter (range (count matrix-a)))] (py-mul [0] (count (py-get matrix-b 0)))))) (letfn [(multiply [i-loop j-loop k-loop matrix-a matrix-b result] (let [result (atom result)] (try (when (py-truthy (py-ge i-loop (count matrix-a))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (when (py-truthy (py-ge j-loop (count (py-get matrix-b 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (multiply (py-add i-loop 1) 0 0 matrix-a matrix-b @result)}))) (when (py-truthy (py-ge k-loop (count matrix-b))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (multiply i-loop (py-add j-loop 1) 0 matrix-a matrix-b @result)}))) (swap! result (fn [__c] (py-assoc-in __c [i-loop j-loop] (py-add (py-get-in __c [i-loop j-loop]) (py-mul (py-get (py-get matrix-a i-loop) k-loop) (py-get (py-get matrix-b k-loop) j-loop)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (multiply i-loop j-loop (py-add k-loop 1) matrix-a matrix-b @result)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))] (multiply 0 0 0 matrix-a matrix-b @result) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(def Matrix nil)

(def matrix-1-to-4 [[1 2] [3 4]])

(def matrix-5-to-8 [[5 6] [7 8]])

(def matrix-5-to-9-high [[5 6] [7 8] [9]])

(def matrix-5-to-9-wide [[5 6] [7 8 9]])

(def matrix-count-up [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(def matrix-unordered [[5 8 1 2] [6 7 3 0] [4 5 9 1] [2 6 10 14]])

(def matrices [matrix-1-to-4 matrix-5-to-8 matrix-5-to-9-high matrix-5-to-9-wide matrix-count-up matrix-unordered])

;; NOTE: const Matrix is a typing alias
