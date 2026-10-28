(ns clojure-algorithms.data-structures.stacks.stack-with-doubly-linked-list
  "Port of data_structures/stacks/stack_with_doubly_linked_list.py.
  Stack backed by a vector (top last). pop returns [value-or-nil new].")

(defn make-stack
  "Empty stack."
  []
  [])

(defn stack-empty?
  "True when empty."
  [st]
  (empty? st))

(defn stack-push
  "Push DATA; returns the new stack."
  [st data]
  (conj (vec st) data))

(defn stack-pop
  "Pop; returns [value-or-nil new-stack]."
  [st]
  (if (empty? st)
    [nil st]
    [(peek (vec st)) (pop (vec st))]))

(defn stack-top
  "Top value, or nil."
  [st]
  (peek (vec st)))

(defn print-stack
  "Print the stack top-first with \"->\" separators."
  [st]
  (println "stack elements are:")
  (println (apply str (mapcat #(str % "->") (reverse st)))))
