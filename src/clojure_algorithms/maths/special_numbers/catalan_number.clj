(ns clojure-algorithms.maths.special-numbers.catalan-number
  "Calculate the nth Catalan number

Source:
    https://en.wikipedia.org/wiki/Catalan_number")

(defn catalan
  "Returns the nth Catalan number. A Catalan number is only defined for
positive integers. Throws an ex-info with :type :TypeError when `number` is
not an integer, and :type :ValueError when it is below 1."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (when (< number 1)
    (throw (ex-info (str "Input value of [number=" number "] must be > 0")
                    {:type :ValueError})))
  (reduce (fn [current i]
            (quot (*' current (- (* 4 i) 2)) (inc i)))
          1
          (range 1 number)))
