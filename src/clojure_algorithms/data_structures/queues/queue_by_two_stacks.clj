(ns clojure-algorithms.data-structures.queues.queue-by-two-stacks
  "Port of data_structures/queues/queue_by_two_stacks.py.
  Queue from two stacks, functionally: {:s1 [...] :s2 [...]}.
  get returns [value new-queue].")

(defn make-queue
  "Queue over COLL."
  ([] (make-queue []))
  ([coll] {:s1 (vec coll) :s2 []}))

(defn queue-len
  "Number of items."
  [{:keys [s1 s2]}]
  (+ (count s1) (count s2)))

(defn- items [{:keys [s1 s2]}]
  (vec (concat (reverse s2) s1)))

(defn- render [v]
  (if (or (string? v) (char? v)) (str "'" v "'") (str v)))

(defn queue-repr
  "Render like the Python repr: Queue((10, 20))."
  [q]
  (let [xs (items q)]
    (cond
      (empty? xs) "Queue(())"
      (= 1 (count xs)) (str "Queue((" (render (first xs)) ",))")
      :else (str "Queue((" (clojure.string/join ", " (map render xs)) "))"))))

(defn put
  "Enqueue ITEM; returns the new queue."
  [q item]
  (update q :s1 conj item))

(defn queue-get
  "Dequeue; returns [value new-queue]."
  [{:keys [s1 s2] :as q}]
  (let [[s1 s2] (if (empty? s2)
                  [[] (vec (concat (reverse s1) s2))]
                  [s1 s2])]
    (if (empty? s2)
      (throw (ex-info "Queue is empty" {:type :index-error}))
      [(peek s2) (assoc q :s1 s1 :s2 (pop s2))])))
