(ns clojure-algorithms.maths.collatz-sequence
  "The Collatz conjecture is a famous unsolved problem in mathematics. Given a starting
  positive integer, define the following sequence:
  - If the current term n is even, then the next term is n/2.
  - If the current term n is odd, then the next term is 3n + 1.
  The conjecture claims that this sequence will always reach 1 for any starting number.

  Reference: https://en.wikipedia.org/wiki/Collatz_conjecture")

(defn- collatz-step
  "Single Collatz step."
  [n]
  (if (even? n) (quot n 2) (+' 1 (*' 3 n))))

(defn collatz-sequence
  "Returns a lazy sequence of the Collatz sequence starting at the positive
  integer `n`, ending at 1."
  [n]
  (when-not (integer? n)
    (throw (ex-info "Sequence only defined for integers" {:type :TypeError})))
  (when (< n 1)
    (throw (ex-info "Sequence only defined for positive integers" {:type :ValueError})))
  (concat (take-while #(not= 1 %) (iterate collatz-step n)) [1]))

(defn main
  "Reads a number from stdin and prints its Collatz sequence."
  []
  (print "Your number: ")
  (flush)
  (let [n (Long/parseLong (read-line))
        sequence (vec (collatz-sequence n))]
    (println sequence)
    (println (str "Collatz sequence from " n " took " (count sequence) " steps."))))
