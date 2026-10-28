(ns clojure-algorithms.scheduling.job-sequencing-with-deadline
  "Job sequencing with deadlines via latest-free-slot greedy placement.

  Port of TheAlgorithms/Python scheduling/job_sequencing_with_deadline.py.
  Jobs are considered highest profit first and each is placed in the
  latest free time slot at or before its deadline.")

(defn job-sequencing-with-deadlines
  "Return [job-count max-profit] for the given jobs.

  `jobs` is a collection of [job-id deadline profit] triples."
  [jobs]
  (let [ordered (sort-by #(nth % 2) > jobs)
        max-deadline (apply max (map #(nth % 1) ordered))]
    (loop [remaining ordered
           slots (vec (repeat max-deadline -1))
           job-count 0
           max-profit 0]
      (if (empty? remaining)
        [job-count max-profit]
        (let [job (first remaining)
              slot (some #(when (= -1 (nth slots %)) %)
                         (range (dec (nth job 1)) -1 -1))]
          (if (nil? slot)
            (recur (rest remaining) slots job-count max-profit)
            (recur (rest remaining)
                   (assoc slots slot (nth job 0))
                   (inc job-count)
                   (+ max-profit (nth job 2)))))))))
