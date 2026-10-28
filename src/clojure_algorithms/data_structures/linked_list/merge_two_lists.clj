(ns clojure-algorithms.data-structures.linked-list.merge-two-lists
  "Port of data_structures/linked_list/merge_two_lists.py.
  Sorted lists backed by sorted vectors.")

(def test-data-odd [3 9 -11 0 7 5 1 -1])
(def test-data-even [4 6 2 0 8 10 3 -2])

(defn make-sorted-list
  "Sorted list of INTS."
  [ints]
  (vec (sort ints)))

(defn ll-str
  "String form with \" -> \" separators."
  [lst]
  (clojure.string/join " -> " (map str lst)))

(defn merge-lists
  "Merge two sorted lists into a new sorted list."
  [sll-one sll-two]
  (make-sorted-list (concat sll-one sll-two)))
