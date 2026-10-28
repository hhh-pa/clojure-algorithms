(ns clojure-algorithms.dynamic-programming.climbing-stairs
  "Distinct ways to climb n stairs taking 1 or 2 steps at a time.
  Ported from TheAlgorithms/Python dynamic_programming/climbing_stairs.py.")

(defn climb-stairs
  "Number of distinct ways to climb `number-of-steps` stairs."
  [number-of-steps]
  (assert (and (integer? number-of-steps) (pos? number-of-steps))
          (str "number_of_steps needs to be positive integer, your input "
               number-of-steps))
  (if (= number-of-steps 1)
    1
    (loop [i 1 previous 1 current 1]
      (if (>= i number-of-steps)
        current
        (recur (inc i) current (+ current previous))))))
