(ns clojure-algorithms.data-structures.queues.monotonic-queue
  "Port of data_structures/queues/monotonic_queue.py.
  Sliding-window maximum via a monotonic deque of indexes.")

(defn max-sliding-window
  "Maximums of each WINDOW-SIZE window of ARR."
  [arr window-size]
  (when (or (empty? arr) (<= window-size 0))
    (throw (ex-info "Invalid input" {:type :value-error})))
  (when (> window-size (count arr))
    (throw (ex-info "Invalid input" {:type :value-error})))
  (loop [i 0 dq [] out []]
    (if (>= i (count arr))
      out
      (let [dq (vec (drop-while #(<= % (- i window-size)) dq))
            dq (vec (reverse (drop-while #(<= (nth arr %) (nth arr i))
                                         (reverse dq))))
            dq (conj dq i)
            out (if (>= i (dec window-size))
                  (conj out (nth arr (first dq)))
                  out)]
        (recur (inc i) dq out)))))
