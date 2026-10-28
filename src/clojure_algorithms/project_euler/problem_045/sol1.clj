(ns clojure-algorithms.project-euler.problem-045.sol1
  "Euler Problem 45: find the next triangle number after 40755 that is also
  pentagonal and hexagonal. Every hexagonal number is triangular, so only
  hexagonal numbers that are also pentagonal are checked.")

(defn hexagonal-num
  "Returns the nth hexagonal number."
  [n]
  (* n (- (* 2 n) 1)))

(defn pentagonal?
  "Returns true if n is pentagonal."
  [n]
  (let [root (Math/sqrt (+ 1 (* 24 n)))]
    (zero? (mod (/ (+ 1 root) 6) 1))))

(defn solution
  "Returns the next triangular, pentagonal and hexagonal number at or after
  the start-th hexagonal number."
  ([] (solution 144))
  ([start]
   (loop [n start]
     (let [num (hexagonal-num n)]
       (if (pentagonal? num) num (recur (inc n)))))))
