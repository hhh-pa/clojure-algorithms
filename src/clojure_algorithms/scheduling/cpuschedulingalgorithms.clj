(ns clojure-algorithms.scheduling.cpuschedulingalgorithms
  "CPU scheduling engine: FCFS, SJF, priority, and round robin simulation.

  Port of the SchedulerEngine half of TheAlgorithms/Python
  scheduling/cpuschedulingalgorithms.py. Processes are maps with :pid,
  :arrival, :burst, and (for priority algorithms) :priority. Each
  simulation returns {:steps :timeline :processes} where steps are
  [time pid-or-nil ready-pids] triples and the timeline holds [time pid]
  pairs; `simulate` additionally computes per-process :stats rows of
  [pid arrival burst completion tat waiting response]. The tkinter /
  matplotlib CPUSchedulerGUI is intentionally not ported.")

(defn- with-remaining
  "Copy `processes` to vectors of maps with :remaining initialized."
  [processes]
  (mapv #(assoc % :remaining (:burst %)) processes))

(defn simulate-fcfs
  "Simulate First Come First Serve; return {:steps :timeline :processes}."
  [processes]
  (let [ordered (vec (sort-by :arrival (with-remaining processes)))]
    (loop [i 0 t 0 steps [] timeline [] procs ordered]
      (if (>= i (count procs))
        {:steps steps :timeline timeline :processes procs}
        (let [t (max t (:arrival (nth procs i)))
              [t steps timeline]
              (loop [k 0 t t steps steps timeline timeline]
                (if (>= k (:burst (nth procs i)))
                  [t steps timeline]
                  (recur (inc k) (inc t)
                         (conj steps [t (:pid (nth procs i)) []])
                         (conj timeline [t (:pid (nth procs i))]))))]
          (recur (inc i) t steps timeline
                 (assoc-in procs [i :completion] t)))))))

(defn- simulate-np
  "Shared non-preemptive loop picking min `pick-key` among ready processes."
  [processes pick-key]
  (let [ordered (vec (sort-by :arrival (with-remaining processes)))]
    (loop [t 0 steps [] timeline [] procs ordered done 0]
      (if (>= done (count procs))
        {:steps steps :timeline timeline :processes procs}
        (let [ready (filter #(and (<= (:arrival (nth procs %)) t)
                                  (not (contains? (nth procs %) :completion)))
                            (range (count procs)))]
          (if (empty? ready)
            (recur (inc t) (conj steps [(inc t) nil []]) timeline procs done)
            (let [idx (apply min-key #(get (nth procs %) pick-key) ready)
                  [t steps timeline]
                  (loop [k 0 t t steps steps timeline timeline]
                    (if (>= k (:burst (nth procs idx)))
                      [t steps timeline]
                      (recur (inc k) (inc t)
                             (conj steps [t (:pid (nth procs idx)) []])
                             (conj timeline [t (:pid (nth procs idx))]))))]
              (recur t steps timeline
                     (assoc-in procs [idx :completion] t)
                     (inc done)))))))))

(defn simulate-sjf-np
  "Simulate non-preemptive Shortest Job First."
  [processes]
  (simulate-np processes :burst))

(defn simulate-priority-np
  "Simulate non-preemptive priority scheduling (lower value first).
  Processes missing :priority default to 0."
  [processes]
  (simulate-np (mapv #(merge {:priority 0} %) processes) :priority))

(defn- simulate-p
  "Shared preemptive loop picking min `pick-key` among ready processes."
  [processes pick-key]
  (let [ordered (vec (sort-by :arrival (with-remaining processes)))]
    (loop [t 0 steps [] timeline [] procs ordered done 0]
      (if (>= done (count procs))
        {:steps steps :timeline timeline :processes procs}
        (let [ready (filter #(and (<= (:arrival (nth procs %)) t)
                                  (pos? (:remaining (nth procs %))))
                            (range (count procs)))]
          (if (empty? ready)
            (recur (inc t) (conj steps [(inc t) nil []]) timeline procs done)
            (let [idx (apply min-key #(get (nth procs %) pick-key) ready)
                  steps (conj steps [t (:pid (nth procs idx)) []])
                  timeline (conj timeline [t (:pid (nth procs idx))])
                  procs (update-in procs [idx :remaining] dec)
                  finished (zero? (:remaining (nth procs idx)))
                  procs (if finished
                          (assoc-in procs [idx :completion] (inc t))
                          procs)]
              (recur (inc t) steps timeline procs
                     (if finished (inc done) done)))))))))

(defn simulate-sjf-p
  "Simulate preemptive Shortest Job First (shortest remaining first)."
  [processes]
  (simulate-p processes :remaining))

(defn simulate-priority-p
  "Simulate preemptive priority scheduling (lower value first).
  Processes missing :priority default to 0."
  [processes]
  (simulate-p (mapv #(merge {:priority 0} %) processes) :priority))

(defn simulate-rr
  "Simulate Round Robin with `quantum`; return {:steps :timeline :processes}."
  [processes quantum]
  (let [ordered (vec (sort-by :arrival (with-remaining processes)))]
    (loop [t 0 queue [] i 0 steps [] timeline [] procs ordered done 0]
      (if (>= done (count procs))
        {:steps steps :timeline timeline :processes procs}
        (let [[queue i] (loop [queue queue i i]
                          (if (and (< i (count procs))
                                   (<= (:arrival (nth procs i)) t))
                            (recur (conj queue i) (inc i))
                            [queue i]))]
          (if (empty? queue)
            (recur (inc t) queue i (conj steps [(inc t) nil []])
                   timeline procs done)
            (let [idx (first queue)
                  queue (vec (rest queue))
                  burst (min quantum (:remaining (nth procs idx)))
                  [t steps timeline queue i procs]
                  (loop [k 0 t t steps steps timeline timeline
                         queue queue i i procs procs]
                    (if (>= k burst)
                      [t steps timeline queue i procs]
                      (let [steps (conj steps
                                        [t (:pid (nth procs idx))
                                         (mapv #(:pid (nth procs %)) queue)])
                            timeline (conj timeline [t (:pid (nth procs idx))])
                            t (inc t)
                            procs (update-in procs [idx :remaining] dec)
                            [queue i]
                            (loop [queue queue i i]
                              (if (and (< i (count procs))
                                       (<= (:arrival (nth procs i)) t))
                                (recur (conj queue i) (inc i))
                                [queue i]))]
                        (recur (inc k) t steps timeline queue i procs))))
                  remaining (:remaining (nth procs idx))]
              (if (pos? remaining)
                (recur t (conj queue idx) i steps timeline procs done)
                (recur t queue i steps timeline
                       (assoc-in procs [idx :completion] t)
                       (inc done))))))))))

(defn calculate-stats
  "Return [pid arrival burst completion tat waiting response] rows for
  simulated `processes` given their execution `timeline`."
  [processes timeline]
  (mapv (fn [{:keys [pid arrival burst completion]}]
          (let [first-exec (or (some (fn [[t p]] (when (= p pid) t)) timeline)
                               arrival)
                tat (- completion arrival)]
            [pid arrival burst completion tat (- tat burst) (- first-exec arrival)]))
        processes))

(defn simulate
  "Run `algorithm` over `processes` with optional `quantum` (default 2).

  `algorithm` is matched case-insensitively: \"fcfs\",
  \"sjf (non-preemptive)\", \"sjf (preemptive)\",
  \"priority (non-preemptive)\", \"priority (preemptive)\", or
  \"round robin\". Returns {:steps :timeline :processes :stats}."
  ([processes algorithm] (simulate processes algorithm 2))
  ([processes algorithm quantum]
   (let [result (case (clojure.string/lower-case algorithm)
                  "fcfs" (simulate-fcfs processes)
                  "sjf (non-preemptive)" (simulate-sjf-np processes)
                  "sjf (preemptive)" (simulate-sjf-p processes)
                  "priority (non-preemptive)" (simulate-priority-np processes)
                  "priority (preemptive)" (simulate-priority-p processes)
                  "round robin" (simulate-rr processes quantum)
                  (throw (ex-info (str "Unknown algorithm: " algorithm)
                                  {:algorithm algorithm})))]
     (assoc result :stats (calculate-stats (:processes result)
                                           (:timeline result))))))
