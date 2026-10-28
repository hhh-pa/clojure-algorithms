(ns clojure-algorithms.data-structures.linked-list.doubly-linked-list-two
  "Port of data_structures/linked_list/doubly_linked_list_two.py.
  Doubly linked list backed by a vector (head first).")

(defn make-list
  "Empty list."
  []
  [])

(defn list-empty?
  "True when the list is empty."
  [lst]
  (empty? lst))

(defn ll-str
  "Space-joined elements."
  [lst]
  (clojure.string/join " " (map str lst)))

(defn list-contains?
  "True when VALUE is present."
  [lst value]
  (boolean (some #(= % value) lst)))

(defn get-head-data
  "Head data, or nil."
  [lst]
  (first lst))

(defn get-tail-data
  "Tail data, or nil."
  [lst]
  (peek (vec lst)))

(defn set-head
  "Prepend NODE data; returns the new list."
  [lst node]
  (vec (cons (if (map? node) (:data node) node) lst)))

(defn set-tail
  "Append NODE data; returns the new list."
  [lst node]
  (conj (vec lst) (if (map? node) (:data node) node)))

(defn insert-value
  "Append VALUE; returns the new list."
  [lst value]
  (conj (vec lst) value))

(defn insert-before-index
  "Insert VALUE before INDEX; returns the new list."
  [lst index value]
  (vec (concat (subvec lst 0 index) [value] (subvec lst index))))

(defn insert-after-index
  "Insert VALUE after INDEX; returns the new list."
  [lst index value]
  (vec (concat (subvec lst 0 (inc index)) [value]
               (subvec lst (inc index)))))

(defn insert-at-position
  "Insert VALUE at 1-based POSITION (append when past the end)."
  [lst position value]
  (if (<= position (count lst))
    (insert-before-index lst (dec position) value)
    (set-tail lst value)))

(defn delete-value
  "Delete the first VALUE; returns the new list. Raises when missing."
  [lst value]
  (let [i (.indexOf ^java.util.List (vec lst) value)]
    (if (neg? i)
      (throw (ex-info "Node not found" {:type :exception}))
      (vec (concat (subvec lst 0 i) (subvec lst (inc i)))))))
