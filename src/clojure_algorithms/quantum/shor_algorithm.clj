(ns clojure-algorithms.quantum.shor-algorithm
  "Classical simulation of Shor's factorization algorithm.

  Port of TheAlgorithms/Python quantum/shor_algorithm.py. Finds the prime
  factors of an integer by period-finding on a^x mod N, computed classically
  here (no qiskit/cirq). The random base guess is seedable via an optional
  seed / java.util.Random argument for deterministic runs.")

(defn- gcd
  "Greatest common divisor of a and b (Euclid's algorithm)."
  [a b]
  (loop [a (Math/abs (long a)) b (Math/abs (long b))]
    (if (zero? b) a (recur b (mod a b)))))

(defn- mod-pow
  "Modular exponentiation: base^exp mod modulus, via squares."
  [base exp modulus]
  (loop [result 1 b (mod base modulus) e exp]
    (if (zero? e)
      result
      (recur (if (odd? e) (mod (* result b) modulus) result)
             (mod (* b b) modulus)
             (quot e 2)))))

(defn period-find
  "Find the period r of num^x mod number, i.e. the smallest r > 0 with
  num^r = 1 (mod number). `num` must be coprime to `number`."
  [num number]
  (loop [start 1]
    (if (= 1 (mod-pow num start number))
      start
      (recur (inc start)))))

(defn shor-algorithm
  "Factor `number` into a vector [p q] of non-trivial factors.

  With one argument uses seed 0 for the random base guesses. With two
  arguments the second is either a long seed or a java.util.Random instance.
  Even inputs short-circuit to [2 n/2]. Unlike the Python source (which
  re-seeds every loop iteration and would loop forever on a bad first
  guess), retries draw fresh values from the RNG so the search terminates."
  ([number] (shor-algorithm number 0))
  ([number seed-or-rng]
   (if (even? number)
     [2 (quot number 2)]
     (let [rng (if (integer? seed-or-rng)
                 (java.util.Random. (long seed-or-rng))
                 seed-or-rng)]
       (loop []
         (let [num (+ 2 (.nextInt rng (- number 2)))
               g (gcd number num)]
           (if (> g 1)
             [g (quot number g)]
             (let [period (period-find num number)
                   start (when (even? period)
                           (mod-pow num (quot period 2) number))]
               (if (and start (not= start (dec number)))
                 (let [p (gcd (dec start) number)
                       q (gcd (inc start) number)]
                   (if (and (> p 1) (> q 1))
                     [p q]
                     (recur)))
                 (recur))))))))))
