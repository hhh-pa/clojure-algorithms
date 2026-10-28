(ns clojure-algorithms.project-euler.problem-014.sol2
  "Project Euler Problem 14: longest Collatz sequence.

  Collatz conjecture: start with any positive integer n. The next term is
  obtained from the previous term as follows:

  If the previous term is even, the next term is one half the previous
  term. If the previous term is odd, the next term is 3 times the
  previous term plus 1. The conjecture states the sequence will always
  reach 1 regardless of starting n.

  Which starting number, under one million, produces the longest chain?

  This solution computes chain lengths recursively with memoization.")

(def ^:private collatz-cache
  "Memoized Collatz sequence lengths."
  (atom {1 1}))

(defn collatz-sequence-length
  "Returns the Collatz sequence length for n."
  [n]
  (if-let [cached (get @collatz-cache n)]
    cached
    (let [next-n (if (even? n) (quot n 2) (inc (* 3 n)))
          length (inc (collatz-sequence-length next-n))]
      (swap! collatz-cache assoc n length)
      length)))

(defn solution
  "Returns the number under n that generates the longest Collatz
  sequence."
  ([] (solution 1000000))
  ([n]
   (second (reduce (fn [a b] (if (pos? (compare b a)) b a))
                   (map (fn [i] [(collatz-sequence-length i) i])
                        (range 1 n))))))
