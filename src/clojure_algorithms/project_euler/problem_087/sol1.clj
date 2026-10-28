(ns clojure-algorithms.project-euler.problem-087.sol1
  "Project Euler 87: prime power triples.

  How many numbers below fifty million can be expressed as the sum of a
  prime square, prime cube, and prime fourth power?")

(defn- sieve
  "Returns a sorted vector of the primes up to n inclusive."
  [n]
  (let [primes (atom (conj (set (range 3 (inc n) 2)) 2))]
    (doseq [p (range 3 (inc n) 2)]
      (when (contains? @primes p)
        (swap! primes #(apply disj % (range (* p p) (inc n) p)))))
    (vec (sort (filter #(<= % n) @primes)))))

(defn solution
  "Returns the number of integers less than limit which can be expressed
  as the sum of a prime square, prime cube, and prime fourth power."
  ([] (solution 50000000))
  ([limit]
   (let [prime-square-limit (long (Math/pow (- limit 24) (/ 1.0 2)))
         primes (sieve prime-square-limit)]
     (count (into #{}
                  (for [p1 primes
                        :let [square (* (long p1) p1)]
                        p2 primes
                        :let [cube (* (long p2) p2 p2)]
                        :while (< (+ square cube) (- limit 16))
                        p3 primes
                        :let [total (+ square cube
                                       (* (long p3) p3 p3 p3))]
                        :while (< total limit)]
                    total))))))
