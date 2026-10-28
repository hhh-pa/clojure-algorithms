(ns clojure-algorithms.project-euler.problem-072.sol2
  "Project Euler 72: counting fractions (prime-sieve variant).

  Counts reduced proper fractions with denominators up to limit by
  sieving primes and applying Euler's product formula to each multiple.")

(defn solution
  "Returns the number of reduced proper fractions with denominators up
  to limit."
  ([] (solution 1000000))
  ([limit]
   (let [primes (atom (conj (set (range 3 limit 2)) 2))]
     (doseq [p (range 3 limit 2)]
       (when (contains? @primes p)
         (swap! primes #(apply disj % (range (* p p) limit p)))))
     (let [phi (double-array (map double (range (inc limit))))]
       (doseq [p @primes n (range p (inc limit) p)]
         (aset phi n (* (aget phi n) (- 1 (/ 1.0 p)))))
       (int (reduce + (map #(aget phi %) (range 2 (inc limit)))))))))
