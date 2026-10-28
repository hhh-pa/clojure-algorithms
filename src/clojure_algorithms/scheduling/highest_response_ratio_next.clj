(ns clojure-algorithms.scheduling.highest-response-ratio-next
  "Highest Response Ratio Next (HRRN) non-preemptive scheduling.

  Port of TheAlgorithms/Python scheduling/highest_response_ratio_next.py.
  The numpy argsort used by the Python source is replaced with pure
  sort-by; inputs are not mutated and results are returned as vectors in
  arrival-time order.")

(defn calculate-turn-arround-time
  "Return the turnaround time of each process in arrival-time order.

  `process-name`, `arrival-time`, and `burst-time` are parallel collections
  and `no-of-process` is the process count. At each step the arrived,
  unfinished process with the highest response ratio
  (burst + (now - arrival)) / burst runs to completion."
  [process-name arrival-time burst-time no-of-process]
  (let [order (vec (sort-by #(nth arrival-time %) (range no-of-process)))
        arrivals (mapv #(nth arrival-time %) order)
        bursts (mapv #(nth burst-time %) order)]
    (loop [current-time 0
           finished (vec (repeat no-of-process 0))
           tat (vec (repeat no-of-process 0))
           done 0]
      (if (>= done no-of-process)
        tat
        (let [i (first (keep-indexed #(when (zero? %2) %1) finished))
              current-time (max current-time (nth arrivals i))
              loc (loop [j 0 ratio 0 loc 0 temp 0]
                    (if (>= j no-of-process)
                      loc
                      (let [temp (if (and (zero? (nth finished j))
                                          (<= (nth arrivals j) current-time))
                                   (/ (+ (nth bursts j)
                                         (- current-time (nth arrivals j)))
                                      (nth bursts j))
                                   temp)]
                        (if (< ratio temp)
                          (recur (inc j) temp j temp)
                          (recur (inc j) ratio loc temp)))))]
          (recur (+ current-time (nth bursts loc))
                 (assoc finished loc 1)
                 (assoc tat loc (+ current-time
                                   (nth bursts loc)
                                   (- (nth arrivals loc))))
                 (inc done)))))))

(defn calculate-waiting-time
  "Return turnaround-time minus burst-time per process.

  `process-name` is accepted for signature parity with the Python source
  and is not otherwise used."
  [process-name turn-arround-time burst-time no-of-process]
  (mapv - turn-arround-time burst-time))
