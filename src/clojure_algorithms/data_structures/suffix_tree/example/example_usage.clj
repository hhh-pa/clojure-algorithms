(ns clojure-algorithms.data-structures.suffix-tree.example.example-usage
  "Port of data_structures/suffix_tree/example/example_usage.py.
  Demo searches over \"monkey banana\". run-demo returns a result map."
  (:require [clojure-algorithms.data-structures.suffix-tree.suffix-tree
             :refer [make-suffix-tree suffix-search]]))

(def demo-text "monkey banana")
(def demo-patterns ["ana" "ban" "na" "xyz" "mon"])

(defn run-demo
  "Search DEMO-PATTERNS in DEMO-TEXT; returns {:pattern found?}."
  ([] (run-demo demo-text demo-patterns))
  ([text patterns]
   (let [tree (make-suffix-tree text)]
     (into {} (map (fn [p] [p (suffix-search tree p)])) patterns))))
