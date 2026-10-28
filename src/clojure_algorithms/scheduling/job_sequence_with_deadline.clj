(ns clojure-algorithms.scheduling.job-sequence-with-deadline
  "Greedy job sequencing: schedule unit-time tasks before their deadlines.

  Port of TheAlgorithms/Python scheduling/job_sequence_with_deadline.py.
  Tasks are considered highest reward first; a task is kept when its
  deadline still leaves a free slot.")

(defrecord Task [task-id deadline reward]
  Object
  (toString [this]
    (str "Task[task-id=" (:task-id this)
         " deadline=" (:deadline this)
         " reward=" (:reward this) "]")))

(defn make-task
  "Build a Task record with the given id, deadline, and reward."
  [task-id deadline reward]
  (->Task task-id deadline reward))

(defn max-tasks
  "Return the ids of the tasks to run for maximum reward.

  `tasks-info` is a collection of [deadline reward] pairs; each task takes
  one unit of time. Tasks are considered highest reward first and kept
  when deadline >= its 1-based slot position."
  [tasks-info]
  (let [tasks (sort-by :reward >
                       (map-indexed (fn [id [deadline reward]]
                                      (->Task id deadline reward))
                                    tasks-info))]
    (vec (keep-indexed (fn [idx task]
                         (when (>= (:deadline task) (inc idx))
                           (:task-id task)))
                       tasks))))
