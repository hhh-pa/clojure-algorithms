(ns clojure-algorithms.data-structures.queues.circular-queue
  "Port of data_structures/queues/circular_queue.py.
  Circular buffer queue. State is {:n :array :front :rear :size}.")

(defn make-circular-queue
  "Circular queue with N slots."
  [n]
  {:n n :array (vec (repeat n nil)) :front 0 :rear 0 :size 0})

(defn q-size
  "Number of elements."
  [q]
  (:size q))

(defn q-empty?
  "True when the queue is empty."
  [q]
  (zero? (:size q)))

(defn q-first
  "Front element, or false when empty (mirrors the Python quirk)."
  [{:keys [array front] :as q}]
  (if (q-empty? q) false (nth array front)))

(defn enqueue
  "Enqueue DATA; returns the new queue."
  [{:keys [n array rear size] :as q} data]
  (if (>= size n)
    (throw (ex-info "QUEUE IS FULL" {:type :exception}))
    (assoc q :array (assoc array rear data)
           :rear (mod (inc rear) n)
           :size (inc size))))

(defn dequeue
  "Dequeue; returns [value new-queue]."
  [{:keys [n array front size] :as q}]
  (if (zero? size)
    (throw (ex-info "UNDERFLOW" {:type :exception}))
    [(nth array front)
     (assoc q :front (mod (inc front) n) :size (dec size))]))
