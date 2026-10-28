(ns clojure-algorithms.matrix.max-area-of-island
  "Given an two dimensional binary matrix grid. An island is a group of 1's (representing\nland) connected 4-directionally (horizontal or vertical.) You may assume all four edges\nof the grid are surrounded by water.  The area of an island is the number of cells with\na value 1 in the island. Return the maximum area of an island in a grid. If there is no\nisland, return 0."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare is-safe depth-first-search find-max-area matrix)

(defn is-safe "Checking whether coordinate (row, col) is valid or not.\n\n>>> is_safe(0, 0, 5, 5)\nTrue\n>>> is_safe(-1,-1, 5, 5)\nFalse"
  [row col rows cols] (let [__b7 (let [__c1 0 __c2 row __c3 rows] (and (py-le __c1 __c2) (py-lt __c2 __c3)))] (if (py-truthy __b7) (let [__c4 0 __c5 col __c6 cols] (and (py-le __c4 __c5) (py-lt __c5 __c6))) __b7)))

(defn depth-first-search "Returns the current area of the island\n\n>>> depth_first_search(0, 0, set(), matrix)\n0"
  [row col seen mat] (let [seen (atom seen)] (let [cols (atom nil) rows (atom nil)] (try (reset! rows (count mat)) (reset! cols (count (py-get mat 0))) (if (py-truthy (let [__b2 (is-safe row col @rows @cols)] (if (py-truthy __b2) (let [__b1 (not (py-in [row col] @seen))] (if (py-truthy __b1) (py-eq (py-get (py-get mat row) col) 1) __b1)) __b2))) (do (swap! seen (fn [__c] (conj (set __c) [row col]))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-add (py-add (py-add (py-add 1 (depth-first-search (py-add row 1) col @seen mat)) (depth-first-search (py-sub row 1) col @seen mat)) (depth-first-search row (py-add col 1) @seen mat)) (depth-first-search row (py-sub col 1) @seen mat))}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn find-max-area "Finds the area of all islands and returns the maximum area.\n\n>>> find_max_area(matrix)\n6"
  [mat] (let [col (atom nil) item (atom nil) line (atom nil) max-area (atom nil) row (atom nil) seen (atom nil)] (try (reset! seen (py-set [])) (reset! max-area 0) (doseq [[__i1 __i2] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) mat))] (reset! row __i1) (reset! line __i2) (doseq [[__i3 __i4] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) @line))] (reset! col __i3) (reset! item __i4) (when (py-truthy (let [__b5 (py-eq @item 1)] (if (py-truthy __b5) (not (py-in [@row @col] @seen)) __b5))) (reset! max-area (py-max [@max-area (depth-first-search @row @col @seen mat)]))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @max-area})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(def matrix [[0 0 1 0 0 0 0 1 0 0 0 0 0] [0 0 0 0 0 0 0 1 1 1 0 0 0] [0 1 1 0 1 0 0 0 0 0 0 0 0] [0 1 0 0 1 1 0 0 1 0 1 0 0] [0 1 0 0 1 1 0 0 1 1 1 0 0] [0 0 0 0 0 0 0 0 0 0 1 0 0] [0 0 0 0 0 0 0 1 1 1 0 0 0] [0 0 0 0 0 0 0 1 1 0 0 0 0]])
