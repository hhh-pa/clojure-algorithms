(ns clojure-algorithms.greedy-methods.activity-selection
  "Activity Selection Problem: greedily select the maximum number of
  non-overlapping activities by always picking the activity that
  finishes earliest. See https://en.wikipedia.org/wiki/Activity_selection_problem")

(defn activity-selection
  "Returns the maximum-size subset of non-overlapping [start end] activities.
  Accepts nil/empty input and returns []."
  [activities]
  (if (empty? activities)
    []
    (let [sorted-acts (sort-by second activities)]
      (reduce (fn [selected act]
                (if (>= (first act) (second (peek selected)))
                  (conj selected act)
                  selected))
              [(first sorted-acts)]
              (rest sorted-acts)))))
