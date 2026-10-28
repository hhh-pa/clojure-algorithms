(ns clojure-algorithms.project-euler.problem-072.sol1
  "Project Euler 72: counting fractions.

  The number of integers between 1 and n that are coprime to n is given
  by Euler's Totient function, phi(n). So the answer is the sum of
  phi(n) for 2 <= n <= 1,000,000. The identity sum of phi(d) for all
  d|n = n is used to find phi(n) with a sieve.

  Note: the Python original uses numpy for the sieve; this port uses a
  plain JVM long array, stdlib only.")

(defn solution
  "Returns the number of elements in the set of reduced proper fractions
  for d <= limit."
  ([] (solution 1000000))
  ([limit]
   (let [phi (long-array (range -1 limit))]
     (doseq [i (range 2 (inc limit))]
       (when (= (aget phi i) (dec i))
         (doseq [j (range (* 2 i) (inc limit) i)]
           (aset phi j (- (aget phi j) (quot (aget phi j) i))))))
     (reduce + (map #(aget phi %) (range 2 (inc limit)))))))
