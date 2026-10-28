(ns clojure-algorithms.project-euler.problem-004.sol1
  "Project Euler Problem 4: largest palindrome product.

  A palindromic number reads the same both ways. The largest palindrome made
  from the product of two 2-digit numbers is 9009 = 91 x 99.

  Find the largest palindrome made from the product of two 3-digit numbers.

  This solution scans downward from n, returning the first palindrome that
  is a product of two 3-digit numbers.")

(defn- palindrome?
  "Returns true when the decimal representation of number reads the same
  both ways."
  [number]
  (let [s (str number)]
    (= s (apply str (reverse s)))))

(defn- product-of-two-3-digit-numbers?
  "Returns true when number is the product of two 3-digit numbers."
  [number]
  (some (fn [divisor]
          (and (zero? (mod number divisor))
               (= 3 (count (str (quot number divisor))))))
        (range 999 99 -1)))

(defn solution
  "Returns the largest palindrome made from the product of two 3-digit
  numbers which is less than n."
  ([] (solution 998001))
  ([n]
   (let [found (some (fn [number]
                       (when (and (palindrome? number)
                                  (product-of-two-3-digit-numbers? number))
                         number))
                     (range (dec n) 9999 -1))]
     (if (nil? found)
       (throw (ex-info "That number is larger than our acceptable range."
                       {:value n}))
       found))))
