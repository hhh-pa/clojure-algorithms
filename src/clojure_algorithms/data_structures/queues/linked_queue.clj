(ns clojure-algorithms.data-structures.queues.linked-queue
  "Port of data_structures/queues/linked_queue.py.
  FIFO queue backed by a vector. get returns [value new-queue].")

(defn make-queue
  "Empty queue."
  []
  [])

(defn q-empty?
  "True when the queue is empty."
  [q]
  (empty? q))

(defn q-str
  "Items joined with \" <- \"."
  [q]
  (clojure.string/join " <- " (map str q)))

(defn put
  "Enqueue ITEM; returns the new queue."
  [q item]
  (conj (vec q) item))

(defn queue-get
  "Dequeue; returns [value new-queue]."
  [q]
  (if (empty? q)
    (throw (ex-info "dequeue from empty queue" {:type :index-error}))
    [(first q) (vec (rest q))]))

(defn queue-clear
  "Empty queue."
  [_]
  [])
