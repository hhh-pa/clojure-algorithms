(ns clojure-algorithms.project-euler.problem-073.sol1
  "Project Euler 73: counting fractions in a range.

  How many fractions lie between 1/3 and 1/2 in the sorted set of
  reduced proper fractions for d <= 12,000?")

(defn- gcd
  "Returns the greatest common divisor of a and b."
  [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn solution
  "Returns the number of fractions lying between 1/3 and 1/2 in the
  sorted set of reduced proper fractions for d <= max-d."
  ([] (solution 12000))
  ([max-d]
   (loop [d 0 fractions-number 0]
     (if (> d max-d)
       fractions-number
       (let [n-start (inc (quot d 3))
             [n-start n-step] (if (even? d)
                                [(+ n-start (- 1 (mod n-start 2))) 2]
                                [n-start 1])]
         (recur (inc d)
                (+ fractions-number
                   (count (filter #(= 1 (gcd % d))
                                  (range n-start (quot (inc d) 2) n-step))))))))))
