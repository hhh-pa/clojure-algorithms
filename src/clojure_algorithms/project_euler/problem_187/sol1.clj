(ns clojure-algorithms.project-euler.problem-187.sol1
  "Project Euler Problem 187: semiprimes.

  A composite is a number containing at least two prime factors. There are
  ten composites below thirty containing precisely two, not necessarily
  distinct, prime factors. Counts how many composite integers below 10^8
  have precisely two prime factors.")

(defn slow-calculate-prime-numbers
  "Returns prime numbers below max-number using the Sieve of Eratosthenes."
  [max-number]
  (if (<= max-number 2)
    []
    (let [is-prime (boolean-array max-number true)]
      (doseq [i (range 2 (inc (long (Math/sqrt (dec max-number)))))]
        (when (aget is-prime i)
          (loop [j (* i i)]
            (when (< j max-number)
              (aset is-prime j false)
              (recur (+ j i))))))
      (vec (filter #(aget is-prime %) (range 2 max-number))))))

(defn calculate-prime-numbers
  "Returns prime numbers below max-number using an odd-only Sieve of
  Eratosthenes."
  [max-number]
  (if (<= max-number 2)
    []
    (let [half (quot max-number 2)
          is-prime (boolean-array half true)]
      (doseq [i (range 3 (inc (long (Math/sqrt (dec max-number)))) 2)]
        (when (aget is-prime (quot i 2))
          (loop [j (quot (* i i) 2)]
            (when (< j half)
              (aset is-prime j false)
              (recur (+ j i))))))
      (vec (cons 2 (for [i (range 1 half)
                         :when (aget is-prime i)]
                     (inc (* 2 i))))))))

(defn slow-solution
  "Returns the number of composite integers below max-number having
  precisely two, not necessarily distinct, prime factors."
  ([] (slow-solution 100000000))
  ([max-number]
   (let [primes (vec (slow-calculate-prime-numbers (quot max-number 2)))]
     (loop [left 0
            right (dec (count primes))
            cnt 0]
       (if (> left right)
         cnt
         (let [right (loop [r right]
                       (if (and (>= r left)
                                (>= (* (nth primes left) (nth primes r))
                                    max-number))
                         (recur (dec r))
                         r))]
           (recur (inc left) right (+ cnt (- right left) 1))))))))

(defn while-solution
  "Returns the number of composite integers below max-number having
  precisely two, not necessarily distinct, prime factors."
  ([] (while-solution 100000000))
  ([max-number]
   (let [primes (vec (calculate-prime-numbers (quot max-number 2)))]
     (loop [left 0
            right (dec (count primes))
            cnt 0]
       (if (> left right)
         cnt
         (let [right (loop [r right]
                       (if (and (>= r left)
                                (>= (* (nth primes left) (nth primes r))
                                    max-number))
                         (recur (dec r))
                         r))]
           (recur (inc left) right (+ cnt (- right left) 1))))))))

(defn solution
  "Returns the number of composite integers below max-number having
  precisely two, not necessarily distinct, prime factors."
  ([] (solution 100000000))
  ([max-number]
   (let [primes (vec (calculate-prime-numbers (quot max-number 2)))
         n (count primes)]
     (loop [left 0
            right (dec n)
            cnt 0]
       (if (or (>= left n) (> left right))
         cnt
         (let [r (loop [r right]
                   (if (or (< r (dec left))
                           (< (* (nth primes left) (nth primes r))
                              max-number))
                     r
                     (recur (dec r))))]
           (recur (inc left) r (+ cnt (- r left) 1))))))))

(defn benchmark
  "Prints timing benchmarks of the three solutions on a modest input."
  []
  (println "Running performance benchmarks...")
  (doseq [[label f] [["slow-solution" slow-solution]
                     ["while-solution" while-solution]
                     ["solution" solution]]]
    (let [start (System/nanoTime)
          _ (f 1000000)
          ms (/ (- (System/nanoTime) start) 1e6)]
      (println (str label " : " ms " ms")))))
