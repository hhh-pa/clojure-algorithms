(ns clojure-algorithms.maths.special-numbers.kaprekar-constant
  "Kaprekar's Constant (6174) - also known as Kaprekar's Routine

Kaprekar's constant is a special number discovered by Indian mathematician
D. R. Kaprekar in 1949. It is notable for the following property:

1. Take any four-digit number with at least two different digits (leading zeros allowed)
2. Arrange the digits in descending order to form the largest possible number
3. Arrange the digits in ascending order to form the smallest possible number
4. Subtract the smaller number from the larger number
5. Repeat the process with the result

This process will always reach 6174 in at most 7 iterations, and once 6174 is
reached, the process will continue to yield 6174.

Example:
    3524 -> 5432 - 2345 = 3087
    3087 -> 8730 - 0378 = 8352
    8352 -> 8532 - 2358 = 6174
    6174 -> 7641 - 1467 = 6174 (repeats)

Reference: https://en.wikipedia.org/wiki/6174_(number)
OEIS: https://oeis.org/A099009"
  (:require [clojure.string :as str]))

(defn- check-number!
  "Throws an ex-info with :type :ValueError unless v is an integer in 0..9999."
  [v]
  (when (or (not (integer? v)) (neg? v) (> v 9999))
    (throw (ex-info (str "Input must be a non-negative integer between 0 and "
                         "9999, got " v)
                    {:type :ValueError}))))

(defn- four-digits
  "The 4-digit zero-padded string form of `number`."
  [number]
  (format "%04d" number))

(defn kaprekar-routine
  "Performs one iteration of Kaprekar's routine on `number` (0-9999) and
returns the result."
  [number]
  (check-number! number)
  (let [digits (four-digits number)]
    (if (= 1 (count (set digits)))
      0
      (let [descending (Integer/parseInt (apply str (reverse (sort digits))))
            ascending (Integer/parseInt (apply str (sort digits)))]
        (- descending ascending)))))

(defn kaprekar-constant
  "Applies Kaprekar's routine until reaching the constant 6174 or running out
of `max-iterations` (default 7). Returns a vector of the iteration count (or
-1 when 6174 was not reached) and the vector of intermediate results."
  ([number] (kaprekar-constant number 7))
  ([number max-iterations]
   (check-number! number)
   (when (or (not (integer? max-iterations)) (< max-iterations 1))
     (throw (ex-info (str "max_iterations must be a positive integer, got "
                          max-iterations)
                     {:type :ValueError})))
   (if (= number 6174)
     [0 [number]]
     (loop [current number, sequence [number], iteration 0]
       (if (>= iteration max-iterations)
         [-1 sequence]
         (let [next (kaprekar-routine current)
               sequence (conj sequence next)]
           (cond
             (= next 6174) [(inc iteration) sequence]
             (zero? next) [-1 sequence]
             :else (recur next sequence (inc iteration)))))))))

(defn is-kaprekar-valid
  "Returns true if `number` (0-9999) is valid for Kaprekar's routine, i.e. it
has at least two different digits."
  [number]
  (check-number! number)
  (> (count (set (four-digits number))) 1))

(defn main
  "Demonstrate Kaprekar's constant with user input and examples."
  []
  (println "Kaprekar's Constant (6174) - Demonstration\n")
  (println (apply str (repeat 50 "=")))
  (doseq [num [3524 1234 6174 9998 1111 5]]
    (println (str "\nStarting number: " (format "%04d" num)))
    (if-not (is-kaprekar-valid num)
      (println "  Invalid: All digits are the same (repdigit)")
      (let [[iterations sequence] (kaprekar-constant num)]
        (cond
          (= iterations -1)
          (println (str "  Did not reach 6174. Sequence: " sequence))
          (zero? iterations)
          (println "  Already at Kaprekar's constant!")
          :else
          (do
            (println (str "  Reached 6174 in " iterations " iteration(s)"))
            (println (str "  Sequence: " (str/join " -> " sequence))))))))
  (println (str "\n" (apply str (repeat 50 "="))))
  (println "\nTry your own number (0-9999):")
  (print "Enter a 4-digit number: ")
  (flush)
  (try
    (let [user-input (Integer/parseInt (str/trim (read-line)))]
      (cond
        (or (neg? user-input) (> user-input 9999))
        (println "Number must be between 0 and 9999")
        (not (is-kaprekar-valid user-input))
        (println "Invalid: All digits are the same!")
        :else
        (let [[iterations sequence] (kaprekar-constant user-input)]
          (println (str "\nSequence: " (str/join " -> " sequence)))
          (when-not (= iterations -1)
            (println (str "Reached 6174 in " iterations " iteration(s)!"))))))
    (catch NumberFormatException _
      (println "Invalid input or no input provided"))))
