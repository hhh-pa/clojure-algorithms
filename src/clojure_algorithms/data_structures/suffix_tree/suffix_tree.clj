(ns clojure-algorithms.data-structures.suffix-tree.suffix-tree
  "Port of data_structures/suffix_tree/suffix_tree.py.
  Suffix trie (all suffixes in a char trie).
  A tree is {:text :root}; nodes are suffix-tree-node maps."
  (:require [clojure-algorithms.data-structures.suffix-tree.suffix-tree-node
             :refer [make-suffix-tree-node]]))

(defn- add-suffix [root suffix index]
  (let [root
        (loop [node root chars (seq suffix) path [:children]]
          (if (empty? chars)
            node
            (let [ch (str (first chars))
                  node (if (get-in node (conj path ch))
                         node
                         (assoc-in node (conj path ch)
                                   (make-suffix-tree-node)))]
              (recur node (rest chars) (conj path ch :children)))))]
    (if (empty? suffix)
      root
      (let [path (vec (interleave (repeat :children)
                                  (map str suffix)))]
        (-> root
            (assoc-in (conj (vec path) :is-end-of-string) true)
            (assoc-in (conj (vec path) :start) index)
            (assoc-in (conj (vec path) :end)
                      (+ index (count suffix) -1)))))))

(defn make-suffix-tree
  "Suffix tree over TEXT."
  [text]
  {:text text
   :root (reduce (fn [root i] (add-suffix root (subs text i) i))
                 (make-suffix-tree-node) (range (count text)))})

(defn suffix-search
  "True when PATTERN occurs in the tree's text."
  [{:keys [root]} pattern]
  (loop [node root chars (seq pattern)]
    (if (empty? chars)
      true
      (let [child (get (:children node) (str (first chars)))]
        (if (nil? child)
          false
          (recur child (rest chars)))))))
