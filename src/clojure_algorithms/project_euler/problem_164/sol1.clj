(ns clojure-algorithms.project-euler.problem-164.sol1
  "Project Euler Problem 164: three consecutive digital sum limit.

  Counts how many 20 digit numbers n (without any leading zero) exist such
  that no three consecutive digits of n have a sum greater than 9, using a
  brute-force recursive solution with caching of intermediate results.")

(defn solve
  "Solves for the remaining digit digits, with previous digit prev1 and
  previous-previous digit prev2, and total sum sum-max. The cache atom
  stores and reuses intermediate results."
  [digit prev1 prev2 sum-max first cache]
  (if (zero? digit)
    1
    (let [cache-key (str digit "," prev1 "," prev2)]
      (if-let [cached (get @cache cache-key)]
        cached
        (let [comb (reduce + 0
                           (for [curr (range (inc (- sum-max prev1 prev2)))
                                 :when (not (and first (zero? curr)))]
                             (solve (dec digit)
                                    curr
                                    prev1
                                    sum-max
                                    false
                                    cache)))]
          (swap! cache assoc cache-key comb)
          comb)))))

(defn solution
  "Solves the problem for n-digits number of digits."
  ([] (solution 20))
  ([n-digits]
   (solve n-digits 0 0 9 true (atom {}))))
