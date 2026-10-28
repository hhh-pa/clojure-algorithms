(ns clojure-algorithms.project-euler.problem-125.sol1
  "Project Euler Problem 125: palindromic consecutive square sums.

  The palindromic number 595 can be written as the sum of consecutive
  squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2. Finds the sum of all
  the numbers less than 10^8 that are both palindromic and can be written as
  the sum of consecutive squares.")

(def limit
  "Upper bound for the palindromic consecutive square sums."
  100000000)

(defn is-palindrome
  "Checks if an integer is palindromic."
  [n]
  (if (zero? (mod n 10))
    false
    (let [s (str n)]
      (= s (apply str (reverse s))))))

(defn solution
  "Returns the sum of all numbers less than lim that are both palindromic
  and can be written as the sum of consecutive squares."
  ([] (solution limit))
  ([lim]
   (loop [first-square 1
          answer #{}]
     (let [sum-squares (+ (* first-square first-square)
                          (* (inc first-square) (inc first-square)))]
       (if (>= sum-squares lim)
         (reduce + 0 answer)
         (recur (inc first-square)
                (loop [last-square (inc first-square)
                       sum-sq sum-squares
                       ans answer]
                  (if (>= sum-sq lim)
                    ans
                    (let [next-last (inc last-square)]
                      (recur next-last
                             (+ sum-sq (* next-last next-last))
                             (if (is-palindrome sum-sq)
                               (conj ans sum-sq)
                               ans)))))))))))
