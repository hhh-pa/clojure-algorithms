(ns clojure-algorithms.maths.sylvester-sequence
  "Calculates the nth number in Sylvester's sequence

Source:
    https://en.wikipedia.org/wiki/Sylvester%27s_sequence")

(defn sylvester
  "The nth number in Sylvester's sequence. Non-integer input raises
AssertionError, mirroring the upstream assert."
  [number]
  (assert (integer? number)
          (str "The input value of [n=" number "] is not an integer"))
  (cond
    (= number 1) 2
    (< number 1) (throw (ex-info (str "The input value of [n=" number "] has to be > 0")
                                 {:type :ValueError}))
    :else (let [num (sylvester (dec number))]
            (inc (*' (dec num) num)))))
