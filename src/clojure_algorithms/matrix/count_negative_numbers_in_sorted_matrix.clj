(ns clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix
  "Given an matrix of numbers in which all rows and all columns are sorted in decreasing\norder, return the number of negative numbers in grid.\n\nReference: https://leetcode.com/problems/count-negative-numbers-in-a-sorted-matrix"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare generate-large-matrix validate-grid find-negative-index count-negatives-binary-search count-negatives-brute-force count-negatives-brute-force-with-break benchmark grid test-grids)

(defn generate-large-matrix ">>> generate_large_matrix() # doctest: +ELLIPSIS\n[[1000, ..., -999], [999, ..., -1001], ..., [2, ..., -1998]]"
  [] (vec (for [i (py-iter (range 1000))] (py-list (range (py-sub 1000 i) (py-sub (py-neg 1000) i) (py-neg 1))))))

(defn validate-grid "Validate that the rows and columns of the grid is sorted in decreasing order.\n>>> for grid in test_grids:\n...     validate_grid(grid)"
  [grid] (assert (py-truthy (every? py-truthy (doall (for [row (py-iter grid)] (py-eq row (if (py-truthy true) (vec (reverse (py-sorted row))) (py-sorted row)))))))) (assert (py-truthy (every? py-truthy (doall (for [col (py-iter (apply map vector grid))] (py-eq (py-list col) (if (py-truthy true) (vec (reverse (py-sorted col))) (py-sorted col)))))))) nil)

(defn find-negative-index "Find the smallest negative index\n\n>>> find_negative_index([0,0,0,0])\n4\n>>> find_negative_index([4,3,2,-1])\n3\n>>> find_negative_index([1,0,-1,-10])\n2\n>>> find_negative_index([0,0,0,-1])\n3\n>>> find_negative_index([11,8,7,-3,-5,-9])\n3\n>>> find_negative_index([-1,-1,-2,-3])\n0\n>>> find_negative_index([5,1,0])\n3\n>>> find_negative_index([-5,-5,-5])\n0\n>>> find_negative_index([0])\n1\n>>> find_negative_index([])\n0"
  [array] (let [left (atom nil) mid (atom nil) num (atom nil) right (atom nil)] (try (reset! left 0) (reset! right (py-sub (count array) 1)) (when (py-truthy (let [__b1 (not (py-truthy array))] (if (py-truthy __b1) __b1 (py-lt (py-get array 0) 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (while (py-truthy (py-gt (py-add @right 1) @left)) (reset! mid (py-floordiv (py-add @left @right) 2)) (reset! num (py-get array @mid)) (when (py-truthy (let [__b2 (py-lt @num 0)] (if (py-truthy __b2) (py-ge (py-get array (py-sub @mid 1)) 0) __b2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @mid}))) (if (py-truthy (py-ge @num 0)) (do (reset! left (py-add @mid 1))) (do (reset! right (py-sub @mid 1))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (count array)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn count-negatives-binary-search "An O(m logn) solution that uses binary search in order to find the boundary between\npositive and negative numbers\n\n>>> [count_negatives_binary_search(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (let [bound (atom nil) i (atom nil) total (atom nil)] (try (reset! total 0) (reset! bound (count (py-get grid 0))) (doseq [__i1 (py-iter (range (count grid)))] (reset! i __i1) (reset! bound (find-negative-index (py-slice (py-get grid @i) nil @bound nil))) (swap! total py-add @bound)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sub (py-mul (count grid) (count (py-get grid 0))) @total)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn count-negatives-brute-force "This solution is O(n^2) because it iterates through every column and row.\n\n>>> [count_negatives_brute_force(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (count (vec (for [row (py-iter grid) number (py-iter row) :when (py-truthy (py-lt number 0))] number))))

(defn count-negatives-brute-force-with-break "Similar to the brute force solution above but uses break in order to reduce the\nnumber of iterations.\n\n>>> [count_negatives_brute_force_with_break(grid) for grid in test_grids]\n[8, 0, 0, 3, 1498500]"
  [grid] (let [i (atom nil) number (atom nil) row (atom nil) total (atom nil)] (try (reset! total 0) (doseq [__i1 (py-iter grid)] (reset! row __i1) (try (doseq [[__i2 __i3] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @row))] (reset! i __i2) (reset! number __i3) (when (py-truthy (py-lt @number 0)) (swap! total py-add (py-sub (count @row) @i)) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @total})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn benchmark "Benchmark our functions next to each other"
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

(def grid (generate-large-matrix))

(def test-grids [[[4 3 2 (py-neg 1)] [3 2 1 (py-neg 1)] [1 1 (py-neg 1) (py-neg 2)] [(py-neg 1) (py-neg 1) (py-neg 2) (py-neg 3)]] [[3 2] [1 0]] [[7 7 6]] [[7 7 6] [(py-neg 1) (py-neg 2) (py-neg 3)]] grid])

;; NOTE: benchmark stubbed (timeit)
