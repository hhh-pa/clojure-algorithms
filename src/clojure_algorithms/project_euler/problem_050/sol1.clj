(ns clojure-algorithms.project-euler.problem-050.sol1
  "Euler Problem 50: which prime below one million can be written as the sum of
  the most consecutive primes?")

(defn prime-sieve
  "Sieve of Eratosthenes: returns all prime numbers below limit."
  [limit]
  (let [sieve (boolean-array limit true)]
    (aset sieve 0 false)
    (when (> limit 1) (aset sieve 1 false))
    (loop [i 3]
      (when (< i (inc (int (Math/sqrt limit))))
        (loop [index (* i 2)]
          (when (< index limit)
            (aset sieve index false)
            (recur (+ index i))))
        (recur (+ i 2))))
    (into [2] (filter (fn [i] (aget sieve i)) (range 3 limit 2)))))

(defn solution
  "Returns the largest prime below ceiling that is the sum of the longest run
  of consecutive primes."
  ([] (solution 1000000))
  ([ceiling]
   (let [primes (vec (prime-sieve ceiling))
         prime-set (set primes)
         prefix (vec (reductions + 0 primes))]
     (loop [i 0 length 0 largest 0]
       (if (>= i (count primes))
         largest
         (let [[length largest]
               (loop [j (+ i length) length length largest largest]
                 (if (>= j (count primes))
                   [length largest]
                   (let [sol (- (prefix j) (prefix i))]
                     (cond
                       (>= sol ceiling) [length largest]
                       (contains? prime-set sol) (recur (inc j) (- j i) sol)
                       :else (recur (inc j) length largest)))))]
           (recur (inc i) length largest)))))))
