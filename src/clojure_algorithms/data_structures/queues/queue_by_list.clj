(ns clojure-algorithms.data-structures.queues.queue-by-list
  "Port of data_structures/queues/queue_by_list.py.
  FIFO queue backed by a vector (front first).
  get returns [value new-queue].")

(defn make-queue
  "Queue over COLL."
  ([] (make-queue []))
  ([coll] (vec coll)))

(defn- render [v]
  (if (or (string? v) (char? v)) (str "'" v "'") (str v)))

(defn queue-repr
  "Render like the Python repr: Queue((10, 20))."
  [q]
  (cond
    (empty? q) "Queue(())"
    (= 1 (count q)) (str "Queue((" (render (first q)) ",))")
    :else (str "Queue((" (clojure.string/join ", " (map render q)) "))")))

(defn put
  "Enqueue ITEM; returns the new queue."
  [q item]
  (conj (vec q) item))

(defn queue-get
  "Dequeue; returns [value new-queue]."
  [q]
  (if (empty? q)
    (throw (ex-info "Queue is empty" {:type :index-error}))
    [(first q) (vec (rest q))]))

(defn queue-rotate
  "Rotate ROTATION times; returns the new queue."
  [q rotation]
  (loop [q (vec q) n rotation]
    (if (<= n 0)
      q
      (recur (conj (vec (rest q)) (first q)) (dec n)))))

(defn get-front
  "Front item."
  [q]
  (if (empty? q)
    (throw (ex-info "Queue is empty" {:type :index-error}))
    (first q)))
