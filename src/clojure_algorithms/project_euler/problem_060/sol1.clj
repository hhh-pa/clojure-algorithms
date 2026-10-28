(ns clojure-algorithms.project-euler.problem-060.sol1
  "Euler Problem 60: find the lowest sum for a set of five primes for which
  any two primes concatenate in either order to produce another prime. The
  search splits primes by their remainder mod 3 (mixed remainders always
  concatenate to a multiple of 3) and backtracks within one class.")

(def ^:private base-mod-1 [3 7 13 19])

(def ^:private base-mod-2 [3 5 11 17])

(def prime?
  "Efficient primality check using 6k +/- 1 optimization, memoized."
  (memoize
   (fn [num]
     (cond
       (< num 2) false
       (#{2 3} num) true
       (or (zero? (mod num 2)) (zero? (mod num 3))) false
       :else (let [root (int (Math/sqrt num))]
               (loop [i 5]
                 (cond
                   (> i root) true
                   (or (zero? (mod num i)) (zero? (mod num (+ i 2)))) false
                   :else (recur (+ i 6)))))))))

(defn sum-digits
  "Returns the digital root of num: recursively sums the digits until a single
  digit is obtained. Throws IllegalArgumentException for negative input."
  [num]
  (when (< num 0)
    (throw (IllegalArgumentException. "num must be non-negative")))
  (if (< num 10)
    num
    (recur (reduce + (map (fn [ch] (Character/digit ch 10)) (str num))))))

(def ^:private prime-pairs (atom {}))

(defn concat-prime?
  "Checks whether the concatenations of num1 and num2 in both orders are prime.
  Results are memoized in an internal cache."
  [num1 num2]
  (let [[a b] (if (> num1 num2) [num2 num1] [num1 num2])
        key [a b]]
    (if (contains? @prime-pairs key)
      (@prime-pairs key)
      (let [result (and (prime? (Long/parseLong (str a b)))
                        (prime? (Long/parseLong (str b a))))]
        (swap! prime-pairs assoc key result)
        result))))

(defn add-prime
  "Appends the next prime congruent to the list's class mod 3 and returns the
  extended vector."
  [primes]
  (let [primes (vec primes)]
    (loop [next-num (+ (peek primes) 3)]
      (if (prime? next-num)
        (conj primes next-num)
        (recur (+ next-num 3))))))

(defn generate-primes
  "Generates the first num-primes primes of the mod-3 class selected by flag
  (1 or 2)."
  ([num-primes] (generate-primes num-primes 1))
  ([num-primes flag]
   (loop [primes (vec (if (= flag 1) base-mod-1 base-mod-2))]
     (if (>= (count primes) num-primes)
       primes
       (recur (add-prime primes))))))

(defn solution
  "Searches for a set of target-size primes with the concat-prime property and
  returns the lowest sum found, or nil. Throws IllegalArgumentException unless
  flag is 1 or 2."
  ([] (solution 5 1000 1))
  ([target-size] (solution target-size 1000 1))
  ([target-size prime-limit] (solution target-size prime-limit 1))
  ([target-size prime-limit flag]
   (when (not (contains? #{1 2} flag))
     (throw (IllegalArgumentException. "flag must be either 1 or 2")))
   (let [primes (generate-primes prime-limit flag)
         search (fn search [chain]
                  (if (= (count chain) target-size)
                    chain
                    (some (fn [p]
                            (when (every? #(concat-prime? p %) chain)
                              (search (conj chain p))))
                          (filter #(> % (peek chain)) primes))))]
     (some (fn [p]
             (let [result (search [p])]
               (when (= (count result) target-size)
                 (reduce + result))))
           primes))))
