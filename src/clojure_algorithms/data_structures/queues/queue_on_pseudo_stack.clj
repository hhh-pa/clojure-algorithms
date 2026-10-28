(ns clojure-algorithms.data-structures.queues.queue-on-pseudo-stack
  "Port of data_structures/queues/queue_on_pseudo_stack.py.
  Queue on a pseudo-stack with the same rotation mechanics.
  State is {:stack [...] :length n}. get returns [value new-queue].")

(defn make-queue
  "Empty queue."
  []
  {:stack [] :length 0})

(defn queue-size
  "Number of items."
  [q]
  (:length q))

(defn queue-str
  "Render like the Python str."
  [{:keys [stack]}]
  (str "<" (clojure.string/join ", " (map pr-str stack)) ">"))

(defn put
  "Enqueue ITEM; returns the new queue."
  [q item]
  (-> q (update :stack conj item) (update :length inc)))

(defn rotate
  "Rotate ROTATION times; returns the new queue."
  [{:keys [stack] :as q} rotation]
  (loop [q q n rotation]
    (if (<= n 0)
      q
      (recur (-> q
                 (update :stack (fn [s] (conj (vec (rest s)) (first s))))
                 (update :length inc)
                 (update :length dec))
             (dec n)))))

(defn queue-get
  "Dequeue; returns [value new-queue]."
  [{:keys [stack length] :as q}]
  (let [q (rotate q 1)
        dequeued (peek (:stack q))
        q (-> q
              (update :stack pop)
              (assoc :length (dec length)))]
    [dequeued (rotate q (dec length))]))

(defn front
  "Front item without removing; returns [value new-queue]."
  [q]
  (let [[v q] (queue-get q)]
    [v (put q v)]))
