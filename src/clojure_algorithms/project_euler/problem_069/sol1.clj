(ns clojure-algorithms.project-euler.problem-069.sol1
  "Project Euler 69: totient maximum.

  Euler's Totient function, phi(n), is used to determine the number of
  numbers less than n which are relatively prime to n. It can be seen
  that n = 6 produces a maximum n/phi(n) for n <= 10. Find the value of
  n <= 1,000,000 for which n/phi(n) is a maximum.")

(defn solution
  "Returns the value of n <= limit for which n/phi(n) is a maximum.
  Precomputes phi(k) for all k <= n using Euler's product formula, then
  finds the k that attains the maximum ratio."
  ([] (solution 1000000))
  ([n]
   (when (<= n 0)
     (throw (IllegalArgumentException.
             "Please enter an integer greater than 0")))
   (let [phi (int-array (range (inc n)))]
     (doseq [number (range 2 (inc n))]
       (when (= (aget phi number) number)
         (aset phi number (dec (aget phi number)))
         (doseq [multiple (range (* number 2) (inc n) number)]
           (aset phi multiple (* (quot (aget phi multiple) number)
                                 (dec number))))))
     (reduce (fn [answer number]
               (if (< (/ answer (aget phi answer))
                      (/ number (aget phi number)))
                 number
                 answer))
             1
             (range 1 (inc n))))))
