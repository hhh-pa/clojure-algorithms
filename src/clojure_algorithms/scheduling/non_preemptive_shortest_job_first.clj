(ns clojure-algorithms.scheduling.non-preemptive-shortest-job-first
  "Non-preemptive Shortest Job First (shortest job next) scheduling.

  Port of TheAlgorithms/Python scheduling/non_preemptive_shortest_job_first.py.
  Whenever the CPU is free, the arrived process with the shortest burst runs
  to completion.")

(defn calculate-waitingtime
  "Return a vector with the waiting time of each process.

  `arrival-time` and `burst-time` are parallel collections and
  `no-of-processes` is the process count."
  [arrival-time burst-time no-of-processes]
  (let [arrivals (vec arrival-time)
        bursts (vec burst-time)]
    (loop [remaining bursts
           waiting (vec (repeat no-of-processes 0))
           completed 0
           total-time 0]
      (if (= completed no-of-processes)
        waiting
        (let [ready (vec (filter #(and (<= (nth arrivals %) total-time)
                                       (pos? (nth remaining %)))
                                 (range no-of-processes)))]
          (if (empty? ready)
            (recur remaining waiting completed (inc total-time))
            (let [target (reduce (fn [a b]
                                   (if (< (nth remaining b) (nth remaining a))
                                     b
                                     a))
                                 ready)
                  total-time (+ total-time (nth bursts target))]
              (recur (assoc remaining target 0)
                     (assoc waiting target
                            (- total-time
                               (nth arrivals target)
                               (nth bursts target)))
                     (inc completed)
                     total-time))))))))

(defn calculate-turnaroundtime
  "Return burst-time + waiting-time per process."
  [burst-time no-of-processes waiting-time]
  (mapv + burst-time waiting-time))
