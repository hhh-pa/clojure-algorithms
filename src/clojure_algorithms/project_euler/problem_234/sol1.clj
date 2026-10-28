(ns clojure-algorithms.project-euler.problem-234.sol1
  "Project Euler Problem 234: semidivisible numbers.

  For n >= 4, lps(n) is the largest prime <= sqrt(n) and ups(n) the smallest
  prime >= sqrt(n). An integer n >= 4 is semidivisible if one of lps(n) and
  ups(n) divides n, but not both. Sums all semidivisible numbers not
  exceeding 999966663333.")

(defn prime-sieve
  "Sieve of Eratosthenes: returns all prime numbers below n."
  [n]
  (let [is-prime (boolean-array n true)]
    (aset is-prime 0 false)
    (aset is-prime 1 false)
    (aset is-prime 2 true)
    (doseq [i (range 3 (long (+ (Math/sqrt n) 1)) 2)]
      (loop [index (* i 2)]
        (when (< index n)
          (aset is-prime index false)
          (recur (+ index i)))))
    (vec (cons 2 (filter #(aget is-prime %) (range 3 n 2))))))

(defn solution
  "Computes the sum of semidivisible numbers up to limit."
  ([] (solution 999966663333))
  ([limit]
   (let [primes (prime-sieve (+ (long (Math/floor (Math/sqrt limit))) 100))]
     (loop [prime-index 0
            last-prime (nth primes 0)
            matches-sum 0]
       (if (> (* last-prime last-prime) limit)
         matches-sum
         (let [next-prime (nth primes (inc prime-index))
               lower-bound (* last-prime last-prime)
               upper-initial (* next-prime next-prime)
               after-lower (loop [current (+ lower-bound last-prime)
                                  total matches-sum]
                             (if (or (>= current upper-initial)
                                     (> current limit))
                               total
                               (recur (+ current last-prime)
                                      (+ total current))))
               upper-bound (loop [u upper-initial]
                             (if (> (- u next-prime) limit)
                               (recur (- u next-prime))
                               u))
               after-upper (loop [current (- upper-bound next-prime)
                                  total after-lower]
                             (if (<= current lower-bound)
                               total
                               (recur (- current next-prime)
                                      (+ total current))))
               after-both (loop [current 0
                                 total after-upper]
                            (cond
                              (or (>= current upper-bound) (> current limit))
                              total
                              (<= current lower-bound)
                              (recur (+ current (* last-prime next-prime))
                                     total)
                              :else
                              (recur (+ current (* last-prime next-prime))
                                     (- total (* 2 current)))))]
           (recur (inc prime-index) next-prime after-both)))))))
