(ns clojure-algorithms.sorts.benchmark-sorts
  "Reference benchmark comparing the sorts in this directory on shared
  random datasets. Ported from TheAlgorithms/Python."
  (:require [clojure-algorithms.sorts.bubble-sort :refer [bubble-sort-iterative]]
            [clojure-algorithms.sorts.cocktail-shaker-sort :refer [cocktail-shaker-sort]]
            [clojure-algorithms.sorts.comb-sort :refer [comb-sort]]
            [clojure-algorithms.sorts.gnome-sort :refer [gnome-sort]]
            [clojure-algorithms.sorts.heap-sort :refer [heap-sort]]
            [clojure-algorithms.sorts.insertion-sort :refer [insertion-sort]]
            [clojure-algorithms.sorts.merge-sort :refer [merge-sort]]
            [clojure-algorithms.sorts.quick-sort :refer [quick-sort]]
            [clojure-algorithms.sorts.selection-sort :refer [selection-sort]]
            [clojure-algorithms.sorts.shell-sort :refer [shell-sort]]
            [clojure-algorithms.sorts.tim-sort :refer [tim-sort]]))

(def sorts
  "Name -> sorting fn. Every fn accepts a collection and returns a vector."
  {"bubble_sort" bubble-sort-iterative
   "cocktail_shaker_sort" cocktail-shaker-sort
   "comb_sort" comb-sort
   "gnome_sort" gnome-sort
   "heap_sort" heap-sort
   "insertion_sort" insertion-sort
   "merge_sort" merge-sort
   "quick_sort" quick-sort
   "selection_sort" selection-sort
   "shell_sort" shell-sort
   "tim_sort" tim-sort})

(defn sorted-asc?
  "True when every element is <= the next one."
  [coll]
  (or (< (count coll) 2)
      (every? (fn [[a b]] (not (pos? (compare a b))))
              (partition 2 1 coll))))

(defn all-sorts-agree?
  "True when every algorithm in `sorts` sorts `data` like clojure.core/sort."
  [data]
  (let [expected (vec (sort data))]
    (every? (fn [[_ f]] (= (vec (f (vec data))) expected)) sorts)))

(defn benchmark
  "Times every algorithm in `sorts` on `data`, `number` repetitions each.
  Returns {name -> elapsed seconds}. Throws on empty data or when
  `number` is not positive."
  ([data] (benchmark data 1))
  ([data number]
   (when (empty? data)
     (throw (IllegalArgumentException. "Please provide a non-empty dataset")))
   (when (<= number 0)
     (throw (IllegalArgumentException.
             "Number of repetitions must be positive")))
   (into {} (map (fn [[name f]]
                   (let [t0 (System/nanoTime)]
                     (dotimes [_ number] (f (vec data)))
                     [name (/ (- (System/nanoTime) t0) 1e9)]))
                 sorts))))

(defn -main
  "Prints a comparison table for datasets of 100, 1 000 and 3 000 ints."
  [& _]
  (let [rng (java.util.Random. 0)
        sizes [100 1000 3000]
        datasets (into {} (map (fn [s]
                                 [s (vec (repeatedly s
                                                     #(.nextInt rng (inc s))))])
                               sizes))]
    (println (str (format "%-22s" "algorithm")
                  (apply str (map #(format "%12d" %) sizes))))
    (println (apply str (repeat 58 "-")))
    (let [per-size (into {} (map (fn [[s d]] [s (benchmark d)]) datasets))]
      (doseq [name (keys sorts)]
        (println (str (format "%-22s" name)
                      (apply str (map #(format "%12.4f"
                                               (get-in per-size [% name]))
                                      sizes))))))
    (println "\nseconds per sort (lower is better); dataset = uniform random ints")))
