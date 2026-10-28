(ns clojure-algorithms.data-structures.linked-list.remove-nth-node-from-end-of-list
  "Port of data_structures/linked_list/remove_nth_node_from_end_of_list.py.
  append PREPENDS (head first) and returns the data.")

(defn make-list
  "Empty list."
  []
  [])

(defn append-item
  "Prepend NEW-DATA; returns [data new-list]."
  [lst new-data]
  [new-data (vec (cons new-data lst))])

(defn view-list
  "String form with \"->\" separators."
  [lst]
  (clojure.string/join "->" (map str lst)))

(defn remove-nth-from-end
  "Remove the POSITION-th node from the end (1-based);
  returns the new list."
  [lst position-from-end]
  (when (empty? lst)
    (throw (ex-info "No element found." {:type :index-error})))
  (let [size (count lst)]
    (when (or (> position-from-end size) (<= position-from-end 0))
      (throw (ex-info "Index out of bounds error." {:type :index-error})))
    (let [idx (- size position-from-end)]
      (vec (concat (subvec lst 0 idx) (subvec lst (inc idx)))))))
