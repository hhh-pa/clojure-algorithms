(ns clojure-algorithms.scheduling.weighted-interval-scheduling
  "Weighted Interval Scheduling via dynamic programming + binary search.

  Port of TheAlgorithms/Python scheduling/weighted_interval_scheduling.py
  (Kleinberg & Tardos, Algorithm Design ch. 6.1). Intervals are
  immutable Interval records built with `make-interval`, which validates
  start < end and non-negative weight like the Python dataclass.")

(defrecord Interval [start end weight])

(defn make-interval
  "Build an Interval, throwing if start >= end or weight is negative."
  [start end weight]
  (when (>= start end)
    (throw (ex-info (str "Start time (" start ") must be strictly less than "
                         "end time (" end ").")
                    {:start start :end end})))
  (when (neg? weight)
    (throw (ex-info (str "Weight (" weight ") must be non-negative.")
                    {:weight weight})))
  (->Interval start end weight))

(defn- bisect-right
  "Count of entries in ascending `sorted-vec` that are <= x."
  [sorted-vec x]
  (loop [lo 0 hi (count sorted-vec)]
    (if (>= lo hi)
      lo
      (let [mid (+ lo (quot (- hi lo) 2))]
        (if (<= (nth sorted-vec mid) x)
          (recur (inc mid) hi)
          (recur lo mid))))))

(defn weighted-interval-scheduling
  "Return [max-weight selected-intervals] for compatible intervals.

  `intervals` is a collection of Interval records. Selected intervals are
  non-overlapping and ordered by finish time; an empty input yields
  [0.0 []]."
  [intervals]
  (if (empty? intervals)
    [0.0 []]
    (let [ordered (vec (sort-by :end intervals))
          n (count ordered)
          end-times (mapv :end ordered)
          predecessors (mapv #(bisect-right end-times (:start (nth ordered %)))
                             (range n))
          dp (loop [j 1 dp (vec (repeat (inc n) 0.0))]
               (if (> j n)
                 dp
                 (let [incl (+ (:weight (nth ordered (dec j)))
                               (nth dp (nth predecessors (dec j))))
                       excl (nth dp (dec j))]
                   (recur (inc j) (assoc dp j (max incl excl))))))
          selected (loop [curr n acc []]
                     (if (zero? curr)
                       acc
                       (let [incl (+ (:weight (nth ordered (dec curr)))
                                     (nth dp (nth predecessors (dec curr))))]
                         (if (> incl (nth dp (dec curr)))
                           (recur (nth predecessors (dec curr))
                                  (conj acc (nth ordered (dec curr))))
                           (recur (dec curr) acc)))))]
      [(nth dp n) (vec (reverse selected))])))
