(ns clojure-algorithms.project-euler.problem-111.sol1
  "Project Euler Problem 111: primes with runs.

  For a sequence of 10 digits, M(4,d) is 8 or 9. Builds a prime list up to
  sqrt(n) used to check primality, then iterates over possible digit
  combinations checking each for primality.")

(defn generate-primes
  "Calculates the list of primes up to and including n."
  [n]
  (if (< n 2)
    []
    (let [primes (boolean-array (inc n) true)]
      (aset primes 0 false)
      (aset primes 1 false)
      (doseq [i (range 2 (inc (int (Math/sqrt (inc n)))))]
        (when (aget primes i)
          (loop [j (* i i)]
            (when (<= j n)
              (aset primes j false)
              (recur (+ j i))))))
      (vec (filter #(aget primes %) (range 2 (inc n)))))))

(defn is-prime
  "Checks if int n is prime using primes-all, a list of relatively small
  primes compared to n."
  [n primes-all]
  (every? #(not (zero? (mod n %))) primes-all))

(defn solution
  "Checks each possible digit combination for primality and returns the sum
  of the primes found."
  ([] (solution 10000000000))
  ([n]
   (let [primes-all (generate-primes (inc (int (Math/sqrt n))))
         n-zeros (- (count (str n)) 3)
         part1 (for [i (range 1 10)
                     j (range 1 10)
                     :let [num (Integer/parseInt
                                (str (str i)
                                     (apply str (repeat n-zeros "0"))
                                     (str j)))]
                     :when (is-prime num primes-all)]
                 num)
         n-dig-1 (- (count (str n)) 2)
         part2 (for [i (range 1 10)
                     j (range (inc n-dig-1))
                     k (range 10)
                     :let [num (Integer/parseInt
                                (str (apply str (repeat j (str i)))
                                     (str k)
                                     (apply str (repeat (- n-dig-1 j) (str i)))))]
                     :when (is-prime num primes-all)]
                 [i num])
         one-digit (set (map first part2))
         n-dig-2 (- (count (str n)) 3)
         two-dig (remove one-digit (range 1 10))
         part3 (for [i two-dig
                     j (range (inc n-dig-2))
                     k (range (inc (- n-dig-2 j)))
                     m1 (range 10)
                     :when (not (and (= m1 0) (= j 0)))
                     m2 (range 10)
                     :let [num (Integer/parseInt
                                (str (apply str (repeat j (str i)))
                                     (str m1)
                                     (apply str (repeat k (str i)))
                                     (str m2)
                                     (apply str
                                            (repeat (- n-dig-2 j k) (str i)))))]
                     :when (is-prime num primes-all)]
                 num)]
     (+ (reduce + 0 part1)
        (reduce + 0 (map second part2))
        (reduce + 0 part3)))))
