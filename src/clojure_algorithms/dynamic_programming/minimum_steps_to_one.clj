(ns clojure-algorithms.dynamic-programming.minimum-steps-to-one
  "Fewest steps from n to 1 using -1, /2 (when even), /3 (when divisible).
  Ported from TheAlgorithms/Python dynamic_programming/minimum_steps_to_one.py.")

(defn min-steps-to-one
  "Minimum steps from positive integer `number` down to 1."
  [number]
  (when (or (not (integer? number)) (<= number 0))
    (throw (IllegalArgumentException.
            (str "n must be greater than 0. Got n = " number))))
  (let [table (atom (assoc (vec (repeat (inc number) (inc number))) 1 0))]
    (doseq [i (range 1 number)]
      (swap! table update (inc i) min (inc (@table i)))
      (when (<= (* i 2) number)
        (swap! table update (* i 2) min (inc (@table i))))
      (when (<= (* i 3) number)
        (swap! table update (* i 3) min (inc (@table i)))))
    (@table number)))
