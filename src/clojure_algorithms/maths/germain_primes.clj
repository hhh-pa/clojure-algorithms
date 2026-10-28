(ns clojure-algorithms.maths.germain-primes
  "A Sophie Germain prime is any prime p, where 2p + 1 is also prime.
The second number, 2p + 1 is called a safe prime.

Examples of Germain primes include: 2, 3, 5, 11, 23.

Their corresponding safe primes: 5, 7, 11, 23, 47.
https://en.wikipedia.org/wiki/Safe_and_Sophie_Germain_primes")

(defn- prime?
  "Primality test in O(sqrt(n)). All primes are of the form 6k +/- 1."
  [n]
  (cond
    (< n 4) (> n 1)
    (or (even? n) (zero? (mod n 3))) false
    :else (let [limit (Math/sqrt (double n))]
            (loop [i 5]
              (cond
                (> i limit) true
                (or (zero? (mod n i)) (zero? (mod n (+ i 2)))) false
                :else (recur (+ i 6)))))))

(defn- check-positive-int
  "Validates the argument for the Germain prime functions."
  [number]
  (when (or (not (integer? number)) (< number 1))
    (throw (ex-info (str "Input value must be a positive integer. Input value: "
                         number)
                    {:type :TypeError}))))

(defn is-germain-prime
  "Checks if input number and 2*number + 1 are prime."
  [number]
  (check-positive-int number)
  (and (prime? number) (prime? (inc (* 2 number)))))

(defn is-safe-prime
  "Checks if input number and (number - 1)/2 are prime.
The smallest safe prime is 5, with the Germain prime 2."
  [number]
  (check-positive-int number)
  (and (zero? (mod (dec number) 2))
       (prime? number)
       (prime? (quot (dec number) 2))))
