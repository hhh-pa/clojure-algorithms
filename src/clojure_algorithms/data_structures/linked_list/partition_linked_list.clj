(ns clojure-algorithms.data-structures.linked-list.partition-linked-list
  "Port of data_structures/linked_list/partition_linked_list.py.
  Stable partition around a value (note the Python typo
  partition_liked_list, kept here as partition-linked-list).")

(defn make-list
  "Empty list."
  []
  [])

(defn ll-str
  "String form with \" --> \" separators."
  [lst]
  (clojure.string/join " --> " (map str lst)))

(defn add-item
  "Insert ITEM at POSITION (default head); returns the new list."
  ([lst item] (add-item lst item 0))
  ([lst item position]
   (when (neg? position)
     (throw (ex-info "Position must be non-negative" {:type :value-error})))
   (when (> position (count lst))
     (throw (ex-info "Out of bounds" {:type :value-error})))
   (vec (concat (subvec lst 0 position) [item] (subvec lst position)))))

(defn partition-linked-list
  "Stable partition: items < VALUE first; returns the new list."
  [lst value]
  (if (empty? lst)
    lst
    (vec (concat (filter #(< % value) lst)
                 (remove #(< % value) lst)))))
