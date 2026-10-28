(ns clojure-algorithms.scheduling.shortest-job-first
  "Preemptive Shortest Job First (shortest remaining time first) scheduling.

  Port of TheAlgorithms/Python scheduling/shortest_job_first.py. The pandas
  table printed by the Python __main__ block is not ported; instead
  `calculate-average-times` returns the averages as a map.")

(defn calculate-waitingtime
  "Return a vector with the waiting time of each process under preemptive
  shortest-job-first: at every time unit the arrived process with the
  smallest remaining burst runs."
  [arrival-time burst-time no-of-processes]
  (let [arrivals (vec arrival-time)
        bursts (vec burst-time)]
    (loop [remaining bursts
           waiting (vec (repeat no-of-processes 0))
           complete 0
           time 0
           minm Long/MAX_VALUE
           short 0
           check false]
      (if (= complete no-of-processes)
        waiting
        (let [[minm short check]
              (loop [j 0 minm minm short short check check]
                (if (>= j no-of-processes)
                  [minm short check]
                  (if (and (<= (nth arrivals j) time)
                           (pos? (nth remaining j))
                           (< (nth remaining j) minm))
                    (recur (inc j) (nth remaining j) j true)
                    (recur (inc j) minm short check))))]
          (if (not check)
            (recur remaining waiting complete (inc time) minm short check)
            (let [remaining (update remaining short dec)
                  left (nth remaining short)
                  minm (if (zero? left) Long/MAX_VALUE left)]
              (if (zero? left)
                (let [finish-time (inc time)
                      w (max (- finish-time
                                (nth arrivals short)
                                (nth bursts short))
                             0)]
                  (recur remaining (assoc waiting short w) (inc complete)
                         (inc time) minm short false))
                (recur remaining waiting complete (inc time)
                       minm short check)))))))))

(defn calculate-turnaroundtime
  "Return burst-time + waiting-time per process."
  [burst-time no-of-processes waiting-time]
  (mapv + burst-time waiting-time))

(defn calculate-average-times
  "Return {:average-waiting w :average-turnaround t} as doubles.

  Pure equivalent of the Python function, which prints the same values."
  [waiting-time turn-arround-time no-of-processes]
  {:average-waiting (double (/ (reduce + waiting-time) no-of-processes))
   :average-turnaround (double (/ (reduce + turn-arround-time) no-of-processes))})
