(ns clojure-algorithms.data-structures.queues.double-ended-queue
  "Port of data_structures/queues/double_ended_queue.py.
  Double-ended queue backed by a vector. Pops return [value new-deque].")

(defn make-deque
  "Deque over COLL."
  ([] (make-deque []))
  ([coll] (vec coll)))

(defn deque-empty?
  "True when the deque is empty."
  [dq]
  (empty? dq))

(defn- render [v]
  (if (or (string? v) (char? v)) (str "'" v "'") (str v)))

(defn deque-str
  "Render like the Python repr."
  [dq]
  (str "[" (clojure.string/join ", " (map render dq)) "]"))

(defn deque-count
  "Number of elements."
  [dq]
  (count dq))

(defn deque-append
  "Append VALUE; returns the new deque."
  [dq value]
  (conj (vec dq) value))

(defn deque-appendleft
  "Prepend VALUE; returns the new deque."
  [dq value]
  (vec (cons value dq)))

(defn deque-extend
  "Append VALUES; returns the new deque."
  [dq values]
  (into (vec dq) values))

(defn deque-extendleft
  "Prepend VALUES (each to the left in order); returns the new deque."
  [dq values]
  (reduce (fn [d v] (vec (cons v d))) (vec dq) values))

(defn deque-pop
  "Remove the right end; returns [value new-deque]."
  [dq]
  (when (empty? dq)
    (throw (ex-info "pop from an empty deque" {:type :index-error})))
  [(peek (vec dq)) (pop (vec dq))])

(defn deque-popleft
  "Remove the left end; returns [value new-deque]."
  [dq]
  (when (empty? dq)
    (throw (ex-info "pop from an empty deque" {:type :index-error})))
  [(first dq) (vec (rest dq))])

(defn deque-clear
  "Empty deque."
  [_]
  [])

(defn deque-copy
  "Copy of the deque."
  [dq]
  (vec dq))

(defn deque-eq?
  "True when contents are equal."
  [dq other]
  (= (vec dq) (vec other)))

(defn deque-reversed
  "Reversed deque."
  [dq]
  (vec (reverse dq)))
