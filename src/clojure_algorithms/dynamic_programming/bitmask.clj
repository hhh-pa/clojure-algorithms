(ns clojure-algorithms.dynamic-programming.bitmask
  "Count ways to assign N tasks to people with bitmask DP.

  Each person does at most one task; each task goes to at most one
  person. `task-performed[i]` lists the 1-based tasks person i can do.
  Ported from TheAlgorithms/Python dynamic_programming/bitmask.py.")

(defn count-no-of-ways
  "Number of ways to distribute `total-tasks` over `task-performed`."
  [task-performed total-tasks]
  (let [by-task (reduce (fn [m [person tasks]]
                          (reduce (fn [acc t] (update acc t (fnil conj []) person))
                                  m tasks))
                        {}
                        (map-indexed vector task-performed))
        final-mask (dec (bit-shift-left 1 (count task-performed)))
        memo (atom {})]
    (letfn [(ways [mask task-no]
              (cond
                (= mask final-mask) 1
                (> task-no total-tasks) 0
                (contains? @memo [mask task-no]) (@memo [mask task-no])
                :else (let [skip (ways mask (inc task-no))
                            take (reduce (fn [total p]
                                           (if (pos? (bit-and mask (bit-shift-left 1 p)))
                                             total
                                             (+ total (ways (bit-or mask (bit-shift-left 1 p))
                                                            (inc task-no)))))
                                         0
                                         (get by-task task-no []))
                            total (+ skip take)]
                        (swap! memo assoc [mask task-no] total)
                        total)))]
      (ways 0 1))))
