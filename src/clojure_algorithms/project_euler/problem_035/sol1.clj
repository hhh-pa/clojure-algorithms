(ns clojure-algorithms.project-euler.problem-035.sol1
  "Euler Problem 35: how many circular primes are there below one million?
  Uses a Sieve of Eratosthenes up to one million, then checks rotations.")

(def ^:private sieve-limit 1000000)

(def ^:private sieve
  (let [s (boolean-array (inc sieve-limit) true)]
    (loop [i 2]
      (when (<= (* i i) sieve-limit)
        (when (aget s i)
          (loop [j (* i i)]
            (when (<= j sieve-limit)
              (aset s j false)
              (recur (+ j i)))))
        (recur (inc i))))
    s))

(defn prime?
  "For 2 <= n <= 1000000, returns true if n is prime."
  [n]
  (boolean (aget sieve n)))

(defn contains-an-even-digit?
  "Returns true if n contains an even digit."
  [n]
  (boolean (some #{\0 \2 \4 \6 \8} (str n))))

(defn find-circular-primes
  "Returns the circular primes below limit."
  ([] (find-circular-primes 1000000))
  ([limit]
   (into [2]
         (comp
          (filter (fn [num] (and (prime? num) (not (contains-an-even-digit? num)))))
          (filter (fn [num]
                    (let [s (str num)]
                      (every? (fn [j]
                                (prime? (Integer/parseInt (str (subs s j) (subs s 0 j)))))
                              (range (count s)))))))
         (range 3 (inc limit) 2))))

(defn solution
  "Returns the count of circular primes below one million."
  []
  (count (find-circular-primes)))
