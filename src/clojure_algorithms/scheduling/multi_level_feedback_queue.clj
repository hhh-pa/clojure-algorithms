(ns clojure-algorithms.scheduling.multi-level-feedback-queue
  "Multi Level Feedback Queue (MLFQ) scheduling.

  Port of TheAlgorithms/Python scheduling/multi_level_feedback_queue.py.
  All queues but the last run one round-robin cycle each; the last queue
  runs first-come-first-served. Processes are immutable MLFQProcess records
  and the scheduler state is an immutable map threaded through every
  function instead of mutated in place.")

(defrecord MLFQProcess [process-name arrival-time stop-time burst-time
                        waiting-time turnaround-time])

(defn make-process
  "Build a fresh MLFQProcess with zero waiting/turnaround time."
  [process-name arrival-time burst-time]
  (->MLFQProcess process-name arrival-time arrival-time burst-time 0 0))

(defn make-mlfq
  "Build the initial MLFQ state map from the queue count, per-queue time
  slices, initial ready queue, and current time."
  [number-of-queues time-slices queue current-time]
  {:number-of-queues number-of-queues
   :time-slices (vec time-slices)
   :ready-queue (vec queue)
   :current-time current-time
   :finish-queue []})

(defn calculate-sequence-of-finish-queue
  "Return the process names in the state's finish queue, in order."
  [state]
  (mapv :process-name (:finish-queue state)))

(defn calculate-waiting-time
  "Return the waiting time of each process in `processes`."
  [processes]
  (mapv :waiting-time processes))

(defn calculate-turnaround-time
  "Return the turnaround time of each process in `processes`."
  [processes]
  (mapv :turnaround-time processes))

(defn calculate-completion-time
  "Return the completion (stop) time of each process in `processes`."
  [processes]
  (mapv :stop-time processes))

(defn calculate-remaining-burst-time-of-processes
  "Return the remaining burst time of each process in `queue`."
  [queue]
  (mapv :burst-time queue))

(defn update-waiting-time
  "Return `process` with waiting time increased by current-time minus its
  stop time, using the state's current time."
  [state process]
  (update process :waiting-time + (- (:current-time state)
                                     (:stop-time process))))

(defn first-come-first-served
  "Run FCFS over `ready-queue`; return [finished-vector updated-state]."
  [state ready-queue]
  (loop [queue (vec ready-queue)
         current-time (:current-time state)
         finished []]
    (if (empty? queue)
      [(vec finished)
       (assoc state
              :current-time current-time
              :finish-queue (into (:finish-queue state) finished))]
      (let [cp (first queue)
            current-time (if (< current-time (:arrival-time cp))
                           (+ current-time (:arrival-time cp))
                           current-time)
            cp (update cp :waiting-time + (- current-time (:stop-time cp)))
            current-time (+ current-time (:burst-time cp))
            cp (assoc cp
                      :burst-time 0
                      :turnaround-time (- current-time (:arrival-time cp))
                      :stop-time current-time)]
        (recur (subvec queue 1) current-time (conj finished cp))))))

(defn round-robin
  "Run one round-robin cycle with `time-slice` over `ready-queue`;
  return [finished-vector remaining-queue updated-state]."
  [state ready-queue time-slice]
  (let [n (count ready-queue)]
    (loop [k 0
           queue (vec ready-queue)
           current-time (:current-time state)
           finished []]
      (if (>= k n)
        [(vec finished) queue
         (assoc state
                :current-time current-time
                :finish-queue (into (:finish-queue state) finished))]
        (let [cp (first queue)
              queue (subvec queue 1)
              current-time (if (< current-time (:arrival-time cp))
                             (+ current-time (:arrival-time cp))
                             current-time)
              cp (update cp :waiting-time + (- current-time (:stop-time cp)))]
          (if (> (:burst-time cp) time-slice)
            (let [current-time (+ current-time time-slice)
                  cp (-> cp
                         (update :burst-time - time-slice)
                         (assoc :stop-time current-time))]
              (recur (inc k) (conj queue cp) current-time finished))
            (let [current-time (+ current-time (:burst-time cp))
                  cp (assoc cp
                            :burst-time 0
                            :stop-time current-time
                            :turnaround-time (- current-time
                                                (:arrival-time cp)))]
              (recur (inc k) queue current-time (conj finished cp)))))))))

(defn multi-level-feedback-queue
  "Run the full MLFQ schedule; return the final state map."
  [state]
  (let [state (loop [i 0 state state]
                (if (>= i (dec (:number-of-queues state)))
                  state
                  (let [[_finished ready state2]
                        (round-robin state
                                     (:ready-queue state)
                                     (nth (:time-slices state) i))]
                    (recur (inc i) (assoc state2 :ready-queue ready)))))
        [_finished state2]
        (first-come-first-served state (:ready-queue state))]
    (assoc state2 :ready-queue [])))
