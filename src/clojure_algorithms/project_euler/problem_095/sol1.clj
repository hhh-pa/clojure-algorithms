(ns clojure-algorithms.project-euler.problem-095.sol1
  "Project Euler 95: amicable chains.

  Find the smallest member of the longest amicable chain with no element
  exceeding one million. Relevant primes are generated, then all
  non-prime numbers up to the maximum are generated from prime product
  combinations while tracking prime factors to compute divisor sums,
  and finally the longest chain is found.")

(defn generate-primes
  "Calculates the vector of primes up to and including max-num."
  [max-num]
  (let [are-primes (boolean-array (inc max-num) true)]
    (when (>= max-num 0) (aset are-primes 0 false))
    (when (>= max-num 1) (aset are-primes 1 false))
    (doseq [i (range 2 (inc (long (Math/sqrt max-num))))]
      (when (aget are-primes i)
        (doseq [j (range (* i i) (inc max-num) i)]
          (aset are-primes j false))))
    (vec (filter #(aget are-primes %) (range (inc max-num))))))

(defn- int-pow
  "Returns base raised to exp as an exact integer."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn multiply
  "Runs over all prime combinations to generate non-prime numbers,
  recording proper-divisor sums into the chain atom. Returns the updated
  map of prime degrees."
  [chain primes min-prime-idx prev-num max-num prev-sum primes-degrees]
  (let [min-prime (nth primes min-prime-idx)
        num (* prev-num min-prime)
        degree (inc (get primes-degrees min-prime 0))
        degrees (assoc primes-degrees min-prime degree)
        new-sum (+ (* prev-sum min-prime)
                   (quot (* (+ prev-sum prev-num) (dec min-prime))
                         (dec (int-pow min-prime degree))))]
    (swap! chain assoc num new-sum)
    (doseq [prime-idx (range min-prime-idx (count primes))
            :while (<= (* (nth primes prime-idx) num) max-num)]
      (multiply chain primes prime-idx num max-num new-sum degrees))
    degrees))

(defn find-longest-chain
  "Finds the smallest element of the longest amicable chain described by
  the divisor-sum vector chain."
  [chain max-num]
  (loop [start 2 max-len 0 min-elem 0]
    (if (>= start (count chain))
      min-elem
      (let [[elem length]
            (loop [elem (nth chain start) visited #{start} length 1]
              (if (and (> elem 1)
                       (<= elem max-num)
                       (not (contains? visited elem)))
                (recur (nth chain elem) (conj visited elem) (inc length))
                [elem length]))]
        (if (and (= elem start) (> length max-len))
          (recur (inc start) length start)
          (recur (inc start) max-len min-elem))))))

(defn solution
  "Runs the calculation for numbers <= max-num."
  ([] (solution 1000000))
  ([max-num]
   (let [primes (generate-primes max-num)
         chain (atom (vec (repeat (inc max-num) 0)))]
     (doseq [[prime-idx prime] (map-indexed vector primes)
             :while (<= (* prime prime) max-num)]
       (multiply chain primes prime-idx 1 max-num 0 {}))
     (find-longest-chain @chain max-num))))
