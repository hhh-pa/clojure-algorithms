(ns clojure-algorithms.maths.juggler-sequence
  "Juggler Sequence.
A Juggler sequence starts with any positive integer n. The next term is
the floor of the square root of n when n is even, and the floor of n
times the square root of n when n is odd.
https://en.wikipedia.org/wiki/Juggler_sequence")

(defn juggler-sequence
  "Returns the Juggler sequence starting at number, ending at 1."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (when (< number 1)
    (throw (ex-info (str "Input value of [number=" number
                         "] must be a positive integer")
                    {:type :ValueError})))
  (loop [n number, seq [number]]
    (if (= n 1)
      seq
      (let [next-n (if (even? n)
                     (long (Math/floor (Math/sqrt (double n))))
                     (let [s (Math/sqrt (double n))]
                       (long (Math/floor (* s s s)))))]
        (recur next-n (conj seq next-n))))))
