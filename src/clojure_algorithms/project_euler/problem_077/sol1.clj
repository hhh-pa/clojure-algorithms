(ns clojure-algorithms.project-euler.problem-077.sol1
  "Project Euler 77: prime summations.

  It is possible to write ten as the sum of primes in exactly five
  different ways. What is the first value which can be written as the
  sum of primes in over five thousand different ways?"
  (:refer-clojure :exclude [partition]))

(def ^:private num-primes 100)

(def ^:private primes
  (let [sieve (conj (set (range 3 num-primes 2)) 2)]
    (reduce (fn [acc p]
              (if (contains? acc p)
                (apply disj acc (range (* p p) num-primes p))
                acc))
            sieve
            (range 3 (long (Math/ceil (Math/sqrt num-primes))) 2))))

(declare partition)

(defn- partition-impl
  [number-to-partition]
  (cond
    (neg? number-to-partition) #{}
    (zero? number-to-partition) #{1}
    :else (into #{}
                (for [prime primes
                      :when (<= prime number-to-partition)
                      sub (partition (- number-to-partition prime))]
                  (* sub prime)))))

(def partition
  "Returns a set of integers corresponding to unique prime partitions of
  n. The unique prime partitions are represented as unique prime
  decompositions, e.g. (7+3) <-> 7*3 = 21."
  (memoize partition-impl))

(defn solution
  "Returns the smallest integer that can be written as the sum of primes
  in over number-unique-partitions unique ways, or nil if there is none
  below 100."
  ([] (solution 5000))
  ([number-unique-partitions]
   (first (filter #(> (count (partition %)) number-unique-partitions)
                  (range 1 num-primes)))))
