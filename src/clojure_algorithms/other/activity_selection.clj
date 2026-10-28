(ns clojure-algorithms.other.activity-selection
  "Activity selection: greedy maximum set of non-overlapping activities.

  Port of TheAlgorithms/Python `other/activity_selection.py`.
  Activities are assumed sorted by finish time.")

(defn max-activities
  "Return indices of a maximum set of non-overlapping activities.
  `start` and `finish` are parallel collections of start/finish times,
  assumed sorted by finish time."
  [start finish]
  (let [start (vec start)
        finish (vec finish)
        n (count finish)]
    (if (zero? n)
      []
      (loop [j 1, i 0, selected [0]]
        (if (>= j n)
          selected
          (if (>= (start j) (finish i))
            (recur (inc j) j (conj selected j))
            (recur (inc j) i selected)))))))

(defn print-max-activities
  "Print a maximum set of activities, mirroring the Python original."
  [start finish]
  (println "The following activities are selected:")
  (doseq [i (max-activities start finish)]
    (print (str i ",")))
  (println))
