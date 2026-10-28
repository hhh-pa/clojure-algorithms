(ns clojure-algorithms.scheduling.round-robin
  "Round Robin scheduling with a fixed time quantum.

  Port of TheAlgorithms/Python scheduling/round_robin.py. Each process
  runs for at most `quantum` (default 2) time units per cycle until its
  burst is exhausted.")

(defn calculate-waiting-times
  "Return a vector with the waiting time of each process for the given
  burst times. With one argument the quantum defaults to 2."
  ([burst-times] (calculate-waiting-times burst-times 2))
  ([burst-times quantum]
   (let [bursts (vec burst-times)]
     (loop [remaining bursts
            waiting (vec (repeat (count bursts) 0))
            t 0]
       (let [[remaining waiting t done]
             (loop [i 0 remaining remaining waiting waiting t t done true]
               (if (>= i (count bursts))
                 [remaining waiting t done]
                 (if (pos? (nth remaining i))
                   (if (> (nth remaining i) quantum)
                     (recur (inc i)
                            (update remaining i - quantum)
                            waiting
                            (+ t quantum)
                            false)
                     (let [t (+ t (nth remaining i))]
                       (recur (inc i)
                              (assoc remaining i 0)
                              (assoc waiting i (- t (nth bursts i)))
                              t
                              false)))
                   (recur (inc i) remaining waiting t done))))]
         (if done
           waiting
           (recur remaining waiting t)))))))

(defn calculate-turn-arround-times
  "Return burst-time + waiting-time per process (up to the shorter input)."
  [burst-times waiting-times]
  (mapv + burst-times waiting-times))
