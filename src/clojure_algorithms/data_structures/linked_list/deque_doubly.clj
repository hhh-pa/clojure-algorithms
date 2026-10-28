(ns clojure-algorithms.data-structures.linked-list.deque-doubly
  "Port of data_structures/linked_list/deque_doubly.py.
  Double-ended queue backed by a vector (front first).
  Removals return [element new-deque].")

(defn make-deque
  "Empty deque."
  []
  [])

(defn deque-empty?
  "True when the deque is empty."
  [dq]
  (empty? dq))

(defn first-element
  "Front element."
  [dq]
  (if (empty? dq)
    (throw (ex-info "List is empty" {:type :exception}))
    (first dq)))

(defn last-element
  "Back element."
  [dq]
  (if (empty? dq)
    (throw (ex-info "List is empty" {:type :exception}))
    (peek (vec dq))))

(defn add-first
  "Prepend ELEMENT; returns the new deque."
  [dq element]
  (vec (cons element dq)))

(defn add-last
  "Append ELEMENT; returns the new deque."
  [dq element]
  (conj (vec dq) element))

(defn remove-first
  "Remove the front; returns [element new-deque]."
  [dq]
  (if (empty? dq)
    (throw (ex-info "remove_first from empty list" {:type :index-error}))
    [(first dq) (vec (rest dq))]))

(defn remove-last
  "Remove the back; returns [element new-deque]."
  [dq]
  (if (empty? dq)
    (throw (ex-info "remove_last from empty list" {:type :index-error}))
    [(peek (vec dq)) (pop (vec dq))]))
