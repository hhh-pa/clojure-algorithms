(ns clojure-algorithms.data-structures.linked-list.xor-linked-list
  "Port of data_structures/linked_list/xor_linked_list.py.
  The XOR-of-addresses mechanics need mutable object identity; in this
  immutable port the list is a plain vector, which preserves the
  observable behavior (insert order and traversal).")

(defn make-list
  "Empty list."
  []
  [])

(defn insert-value
  "Append VALUE; returns the new list."
  [lst value]
  (conj (vec lst) value))

(defn to-list
  "Forward traversal as a vector."
  [lst]
  (vec lst))
