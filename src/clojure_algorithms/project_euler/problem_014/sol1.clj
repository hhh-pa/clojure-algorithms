(ns clojure-algorithms.project-euler.problem-014.sol1
  "Project Euler Problem 14: longest Collatz sequence.

  The following iterative sequence is defined for the set of positive
  integers:

      n -> n/2 (n is even)
      n -> 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following
  sequence:

      13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

  It can be seen that this sequence (starting at 13 and finishing at 1)
  contains 10 terms. Although it has not been proved yet (Collatz
  Problem), it is thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  This solution memoizes chain lengths in a map.")

(defn solution
  "Returns the number under n that generates the longest sequence using
  the formula n -> n/2 (n is even), n -> 3n + 1 (n is odd)."
  ([] (solution 1000000))
  ([n]
   (loop [input 2 largest-number 1 pre-counter 1 counters {1 1}]
     (if (>= input n)
       largest-number
       (let [counter (loop [number input counter 0]
                       (cond (contains? counters number)
                             (+ counter (counters number))
                             (even? number)
                             (recur (quot number 2) (inc counter))
                             :else
                             (recur (inc (* 3 number)) (inc counter))))
             counters (if (contains? counters input)
                        counters
                        (assoc counters input counter))]
         (if (> counter pre-counter)
           (recur (inc input) input counter counters)
           (recur (inc input) largest-number pre-counter counters)))))))
