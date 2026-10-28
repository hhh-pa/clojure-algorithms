(ns clojure-algorithms.data-structures.stacks.stack-with-singly-linked-list
  "Port of data_structures/stacks/stack_with_singly_linked_list.py.
  Stack backed by a vector (top first). pop returns [value new-stack].")

(defn make-stack
  "Empty stack."
  []
  [])

(defn stack-empty?
  "True when empty."
  [st]
  (empty? st))

(defn stack-str
  "Items joined with \"->\", top first."
  [st]
  (clojure.string/join "->" (map str st)))

(defn stack-push
  "Push ELEMENT; returns the new stack."
  [st element]
  (vec (cons element st)))

(defn stack-pop
  "Pop; returns [value new-stack]."
  [st]
  (if (empty? st)
    (throw (ex-info "pop from empty stack" {:type :index-error}))
    [(first st) (vec (rest st))]))

(defn stack-top
  "Top value, or nil."
  [st]
  (first st))

(defn stack-clear
  "Empty stack."
  [_]
  [])
