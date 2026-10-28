(ns clojure-algorithms.project-euler.problem-070.sol1
  "Project Euler 70: totient permutation.

  Interestingly, phi(87109) = 79180, and it can be seen that 87109 is a
  permutation of 79180. Find the value of n, 1 < n < 10^7, for which
  phi(n) is a permutation of n and the ratio n/phi(n) produces a minimum.

  Note: the Python original uses numpy for the totient sieve; this port
  implements the same sieve with a plain JVM int array, stdlib only.")

(defn get-totients
  "Calculates a vector of totients from 0 to max-one exclusive, using
  the definition of Euler's product formula."
  [max-one]
  (let [totients (int-array (range max-one))]
    (doseq [i (range 2 max-one)]
      (when (= (aget totients i) i)
        (doseq [j (range i max-one i)]
          (aset totients j (- (aget totients j) (quot (aget totients j) i))))))
    (vec totients)))

(defn has-same-digits
  "Returns true if num1 and num2 have the same frequency of every digit,
  false otherwise."
  [num1 num2]
  (= (sort (str num1)) (sort (str num2))))

(defn solution
  "Finds the value of n from 1 to max-n for which phi(n) is a permutation
  of n and n/phi(n) produces a minimum. Cross multiplication avoids
  dividing by zero when comparing fractions."
  ([] (solution 10000000))
  ([max-n]
   (let [totients (get-totients (inc max-n))]
     (loop [i 2 min-numerator 1 min-denominator 0]
       (if (> i max-n)
         min-numerator
         (let [t (nth totients i)]
           (if (and (< (* (long i) min-denominator)
                       (* (long min-numerator) t))
                    (has-same-digits i t))
             (recur (inc i) i t)
             (recur (inc i) min-numerator min-denominator))))))))
