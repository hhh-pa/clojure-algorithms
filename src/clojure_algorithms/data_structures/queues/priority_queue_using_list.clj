(ns clojure-algorithms.data-structures.queues.priority-queue-using-list
  "Port of data_structures/queues/priority_queue_using_list.py.
  Fixed (3-level) and element (min-first) priority queues.
  Dequeues return [value new-queue].")

(defn make-fixed-queue
  "Fixed priority queue with 3 levels."
  []
  {:queues [[] [] []]})

(defn fp-str
  "Render like the Python str."
  [{:keys [queues]}]
  (clojure.string/join "\n"
                       (map-indexed (fn [i q] (str "Priority " i ": [" (clojure.string/join ", " (map str q)) "]"))
                                    queues)))

(defn fp-enqueue
  "Enqueue DATA at PRIORITY (0-2); returns the new queue."
  [{:keys [queues] :as fpq} priority data]
  (if-not (<= 0 priority 2)
    (throw (ex-info "Valid priorities are 0, 1, and 2" {:type :value-error}))
    (if (>= (count (nth queues priority)) 100)
      (throw (ex-info "Maximum queue size is 100" {:type :overflow-error}))
      (update-in fpq [:queues priority] conj data))))

(defn fp-dequeue
  "Dequeue the highest-priority FIFO item; returns [value new-queue]."
  [{:keys [queues] :as fpq}]
  (if-let [i (first (keep-indexed (fn [i q] (when (seq q) i)) queues))]
    [(first (nth queues i)) (update-in fpq [:queues i] (comp vec rest))]
    (throw (ex-info "All queues are empty" {:type :underflow-error}))))

(defn make-element-queue
  "Element priority queue."
  []
  {:queue []})

(defn ep-str
  "Render like the Python str."
  [{:keys [queue]}]
  (str "[" (clojure.string/join ", " (map str queue)) "]"))

(defn ep-enqueue
  "Enqueue DATA; returns the new queue."
  [{:keys [queue] :as epq} data]
  (if (= 100 (count queue))
    (throw (ex-info "Maximum queue size is 100" {:type :overflow-error}))
    (update epq :queue conj data)))

(defn ep-dequeue
  "Dequeue the minimum item (FIFO ties); returns [value new-queue]."
  [{:keys [queue] :as epq}]
  (if (empty? queue)
    (throw (ex-info "The queue is empty" {:type :underflow-error}))
    (let [data (apply min queue)
          idx (.indexOf ^java.util.List queue data)]
      [data (assoc epq :queue (vec (concat (subvec queue 0 idx)
                                           (subvec queue (inc idx)))))])))
