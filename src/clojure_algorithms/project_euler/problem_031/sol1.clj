(ns clojure-algorithms.project-euler.problem-031.sol1
  "Euler Problem 31: in how many different ways can 2 pounds be made using any
  number of 1p, 2p, 5p, 10p, 20p, 50p, 100p and 200p coins? Solved here with
  one recursive counter per coin denomination.")

(declare two-pence five-pence ten-pence twenty-pence fifty-pence one-pound two-pound)

(defn one-pence
  "Returns 1: there is exactly one way to make any amount with 1p coins."
  []
  1)

(defn two-pence
  "Returns the number of ways to make x pence using 1p and 2p coins."
  [x]
  (if (< x 0) 0 (+ (two-pence (- x 2)) (one-pence))))

(defn five-pence
  "Returns the number of ways to make x pence using coins up to 5p."
  [x]
  (if (< x 0) 0 (+ (five-pence (- x 5)) (two-pence x))))

(defn ten-pence
  "Returns the number of ways to make x pence using coins up to 10p."
  [x]
  (if (< x 0) 0 (+ (ten-pence (- x 10)) (five-pence x))))

(defn twenty-pence
  "Returns the number of ways to make x pence using coins up to 20p."
  [x]
  (if (< x 0) 0 (+ (twenty-pence (- x 20)) (ten-pence x))))

(defn fifty-pence
  "Returns the number of ways to make x pence using coins up to 50p."
  [x]
  (if (< x 0) 0 (+ (fifty-pence (- x 50)) (twenty-pence x))))

(defn one-pound
  "Returns the number of ways to make x pence using coins up to 100p."
  [x]
  (if (< x 0) 0 (+ (one-pound (- x 100)) (fifty-pence x))))

(defn two-pound
  "Returns the number of ways to make x pence using coins up to 200p."
  [x]
  (if (< x 0) 0 (+ (two-pound (- x 200)) (one-pound x))))

(defn solution
  "Returns the number of different ways n pence can be made using any number
  of coins."
  ([] (solution 200))
  ([n] (two-pound n)))
