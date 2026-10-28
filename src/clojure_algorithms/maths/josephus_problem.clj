(ns clojure-algorithms.maths.josephus-problem
  "The Josephus problem is a famous theoretical problem related to a certain
  counting-out game. This module provides functions to solve the Josephus
  problem for num_people and a step_size.

  The Josephus problem is defined as follows:
  - num_people are standing in a circle.
  - Starting with a specified person, you count around the circle,
    skipping a fixed number of people (step_size).
  - The person at which you stop counting is eliminated from the circle.
  - The counting continues until only one person remains.

  For more information about the Josephus problem, refer to:
  https://en.wikipedia.org/wiki/Josephus_problem")

(defn- check-positive-ints
  [num-people step-size]
  (when-not (and (integer? num-people) (integer? step-size)
                 (pos? num-people) (pos? step-size))
    (throw (ex-info "num_people or step_size is not a positive integer."
                    {:type :ValueError}))))

(defn josephus-recursive
  "Solves the Josephus problem for num_people and a step_size recursively.
  Returns the position of the last person remaining (0-based index)."
  [num-people step-size]
  (check-positive-ints num-people step-size)
  ;; Same recurrence as upstream (f(1) = 0;
  ;; f(n) = (f(n-1) + step_size) % n), evaluated bottom-up.
  (loop [i 1 acc 0]
    (if (> i num-people)
      acc
      (recur (inc i) (mod (+ acc step-size) i)))))

(defn find-winner
  "Finds the winner of the Josephus problem for num_people and a step_size.
  Returns the position of the last person remaining (1-based index)."
  [num-people step-size]
  (inc (josephus-recursive num-people step-size)))

(defn josephus-iterative
  "Solves the Josephus problem for num_people and a step_size iteratively.
  Returns the position of the last person standing (1-based index)."
  [num-people step-size]
  (loop [circle (vec (range 1 (inc num-people)))
         current 0]
    (if (<= (count circle) 1)
      (nth circle 0)
      (let [current (mod (+ current step-size -1) (count circle))]
        (recur (vec (concat (subvec circle 0 current)
                            (subvec circle (inc current))))
               current)))))
