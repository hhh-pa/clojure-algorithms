(ns clojure-algorithms.project-euler.problem-145.sol1
  "Project Euler Problem 145: reversible numbers.

  Some positive integers n have the property that n + reverse(n) consists
  entirely of odd digits; for instance 36 + 63 = 99. Such numbers are called
  reversible. There are 120 reversible numbers below one thousand. Counts how
  many reversible numbers are below one billion (10^9).")

(def even-digits
  "Even decimal digits."
  [0 2 4 6 8])

(def odd-digits
  "Odd decimal digits."
  [1 3 5 7 9])

(defn slow-reversible-numbers
  "Counts the number of reversible numbers of the given length. Iterates
  over possible digits considering the parity of the current sum remainder."
  [remaining-length remainder digits length]
  (cond
    (zero? remaining-length)
    (if (or (zero? (first digits)) (zero? (peek (vec digits))))
      0
      (loop [i (dec (quot length 2))
             rem remainder]
        (if (neg? i)
          1
          (let [r (+ rem (nth digits i) (nth digits (- length i 1)))]
            (if (even? r)
              0
              (recur (dec i) (quot r 10)))))))
    (= 1 remaining-length)
    (if (even? remainder)
      0
      (reduce + 0
              (for [digit (range 10)]
                (slow-reversible-numbers
                 0
                 (quot (+ remainder (* 2 digit)) 10)
                 (assoc (vec digits) (quot length 2) digit)
                 length))))
    :else
    (reduce + 0
            (for [digit1 (range 10)
                  :let [with-first (assoc (vec digits)
                                          (dec (quot (+ length remaining-length) 2))
                                          digit1)
                        other-parity-digits (if (even? (+ remainder digit1))
                                              odd-digits
                                              even-digits)]
                  digit2 other-parity-digits]
              (slow-reversible-numbers
               (- remaining-length 2)
               (quot (+ remainder digit1 digit2) 10)
               (assoc with-first
                      (quot (- length remaining-length) 2)
                      digit2)
               length)))))

(defn slow-solution
  "Counts reversible numbers below 10^max-power using the slow recursion."
  ([] (slow-solution 9))
  ([max-power]
   (reduce + 0
           (for [length (range 1 (inc max-power))]
             (slow-reversible-numbers length
                                      0
                                      (vec (repeat length 0))
                                      length)))))

(defn reversible-numbers
  "Counts the number of reversible numbers of the given length. There exist
  no reversible 1, 5, 9, 13 (i.e. 4k+1) digit numbers."
  [remaining-length remainder digits length]
  (if (zero? (mod (dec length) 4))
    0
    (slow-reversible-numbers remaining-length remainder digits length)))

(defn solution
  "Counts reversible numbers below 10^max-power."
  ([] (solution 9))
  ([max-power]
   (reduce + 0
           (for [length (range 1 (inc max-power))]
             (reversible-numbers length
                                 0
                                 (vec (repeat length 0))
                                 length)))))

(defn benchmark
  "Prints timing benchmarks of slow-solution versus solution on a small
  input."
  []
  (println "Running performance benchmarks...")
  (let [start (System/nanoTime)
        _ (slow-solution 6)
        slow-ms (/ (- (System/nanoTime) start) 1e6)
        start (System/nanoTime)
        _ (solution 6)
        fast-ms (/ (- (System/nanoTime) start) 1e6)]
    (println (str "slow-solution : " slow-ms " ms"))
    (println (str "solution      : " fast-ms " ms"))))
