(ns clojure-algorithms.project-euler.problem-051.sol1
  "Euler Problem 51: find the smallest prime which, by replacing part of the
  number with the same digit, is part of an eight prime value family.")

(defn prime-sieve
  "Sieve of Eratosthenes: returns all prime numbers below n."
  [n]
  (let [sieve (boolean-array n true)]
    (aset sieve 0 false)
    (when (> n 1) (aset sieve 1 false))
    (loop [i 3]
      (when (< i (inc (int (Math/sqrt n))))
        (loop [index (* i 2)]
          (when (< index n)
            (aset sieve index false)
            (recur (+ index i))))
        (recur (+ i 2))))
    (into [2] (filter (fn [i] (aget sieve i)) (range 3 n 2)))))

(defn digit-replacements
  "Returns all possible families of digit replacements in number: for each
  repeating digit, the ten numbers formed by replacing every occurrence of it
  with 0-9."
  [number]
  (let [s (str number)
        counts (frequencies s)
        duplicates (filter (fn [[_ c]] (> c 1)) counts)]
    (mapv (fn [[dup _]]
            (mapv (fn [d] (Integer/parseInt (.replace s (str dup) (str d))))
                  (range 10)))
          duplicates)))

(defn solution
  "Returns the smallest prime in the first family of exactly family-length
  primes found, scanning candidate primes in ascending order. Returns -1 if no
  such family exists."
  ([] (solution 8))
  ([family-length]
   (let [primes (into #{}
                      (filter (fn [x]
                                (>= (- (count (str x))
                                       (count (set (str x))))
                                    3)))
                      (prime-sieve 1000000))]
     (loop [remaining (sort primes)
            checked #{}]
       (if (empty? remaining)
         -1
         (let [prime (first remaining)]
           (if (contains? checked prime)
             (recur (rest remaining) checked)
             (let [replacements (digit-replacements prime)]
               (if-let [found (some (fn [family]
                                      (let [in-family (filter primes family)]
                                        (when (= (count in-family) family-length)
                                          (apply min in-family))))
                                    replacements)]
                 found
                 (recur (rest remaining)
                        (into checked (mapcat identity replacements))))))))))))
