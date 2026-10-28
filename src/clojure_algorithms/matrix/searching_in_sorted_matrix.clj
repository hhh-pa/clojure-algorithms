(ns clojure-algorithms.matrix.searching-in-sorted-matrix
  "Port of matrix/searching_in_sorted_matrix.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare search-in-a-sorted-matrix main)

(defn search-in-a-sorted-matrix ">>> search_in_a_sorted_matrix(\n...     [[2, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]], 3, 3, 5)\nKey 5 found at row- 1 column- 2\n>>> search_in_a_sorted_matrix(\n...     [[2, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]], 3, 3, 21)\nKey 21 not found\n>>> search_in_a_sorted_matrix(\n...     [[2.1, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]], 3, 3, 2.1)\nKey 2.1 found at row- 1 column- 1\n>>> search_in_a_sorted_matrix(\n...     [[2.1, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]], 3, 3, 2.2)\nKey 2.2 not found"
  [mat m n key] (let [i (atom nil) j (atom nil)] (try (let [__d1 (vec [(py-sub m 1) 0])] (reset! i (nth __d1 0)) (reset! j (nth __d1 1))) (while (py-truthy (let [__b2 (py-ge @i 0)] (if (py-truthy __b2) (py-lt @j n) __b2))) (when (py-truthy (py-eq key (py-get (py-get mat @i) @j))) (println (clojure.string/join " " [(py-str (str "Key " (py-str key) " found at row- " (py-str (py-add @i 1)) " column- " (py-str (py-add @j 1))))])) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (if (py-truthy (py-lt key (py-get (py-get mat @i) @j))) (do (swap! i py-sub 1)) (do (swap! j py-add 1)))) (println (clojure.string/join " " [(py-str (str "Key " (py-str key) " not found"))])) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [] (let [mat (atom nil) x (atom nil)] (reset! mat [[2 5 7] [4 8 13] [9 11 15] [12 17 20]]) (reset! x (py-int (do (print "Enter the element to be searched:") (flush) (read-line)) nil)) (println (clojure.string/join " " [(py-str @mat)])) (search-in-a-sorted-matrix @mat (count @mat) (count (py-get @mat 0)) @x) nil))
