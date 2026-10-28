(ns clojure-algorithms.data-structures.linked-list.middle-element-of-linked-list
  "Port of data_structures/linked_list/middle_element_of_linked_list.py.
  Push prepends (head first). push returns [data new-list].")

(defn make-list
  "Empty list."
  []
  [])

(defn push
  "Prepend NEW-DATA; returns [data new-list]."
  [lst new-data]
  [new-data (vec (cons new-data lst))])

(defn middle-element
  "Middle element via slow/fast pointers; nil (printing a note) when empty."
  [lst]
  (if (empty? lst)
    (do (println "No element found.") nil)
    (loop [slow 0 fast 0]
      (if (and (< fast (count lst)) (< (inc fast) (count lst)))
        (recur (inc slow) (+ fast 2))
        (nth lst slow)))))
