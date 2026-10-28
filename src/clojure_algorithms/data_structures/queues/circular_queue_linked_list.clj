(ns clojure-algorithms.data-structures.queues.circular-queue-linked-list
  "Port of data_structures/queues/circular_queue_linked_list.py.
  Bounded FIFO queue. State is {:capacity [...] :items [...]}.")

(defn make-circular-queue
  "Circular queue with LIMIT slots (default 6)."
  ([] (make-circular-queue 6))
  ([limit] {:capacity limit :items []}))

(defn q-empty?
  "True when the queue is empty."
  [q]
  (empty? (:items q)))

(defn q-str
  "Space-joined items."
  [q]
  (clojure.string/join " " (map str (:items q))))

(defn enqueue
  "Enqueue DATA; returns the new queue."
  [{:keys [capacity items] :as q} data]
  (if (>= (count items) capacity)
    (throw (ex-info "Full Queue" {:type :exception}))
    (update q :items conj data)))

(defn dequeue
  "Dequeue; returns [value new-queue]."
  [{:keys [items] :as q}]
  (if (empty? items)
    (throw (ex-info "Empty Queue" {:type :exception}))
    [(first items) (assoc q :items (vec (rest items)))]))

(defn q-first
  "Front element."
  [{:keys [items]}]
  (if (empty? items)
    (throw (ex-info "Empty Queue" {:type :exception}))
    (first items)))
