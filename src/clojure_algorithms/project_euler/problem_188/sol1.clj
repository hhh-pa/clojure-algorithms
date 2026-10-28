(ns clojure-algorithms.project-euler.problem-188.sol1
  "Project Euler Problem 188: hyperexponentiation.

  The hyperexponentiation or tetration of a number a by a positive integer b
  is recursively defined by a^^1 = a and a^^(k+1) = a^(a^^k). Finds the last
  8 digits of 1777^^1855.")

(defn mod-expt
  "Returns the modular exponentiation, that is the value of
  base^exponent mod modulo-value, without calculating the actual number."
  [base exponent modulo-value]
  (cond
    (= exponent 1) base
    (even? exponent)
    (let [x (mod (mod-expt base (quot exponent 2) modulo-value) modulo-value)]
      (mod (* x x) modulo-value))
    :else
    (mod (* base (mod-expt base (dec exponent) modulo-value)) modulo-value)))

(defn solution
  "Returns the last digits digits of the hyperexponentiation of base by
  height, i.e. the number base^^height."
  ([] (solution 1777 1855 8))
  ([base height] (solution base height 8))
  ([base height digits]
   (let [modulus (long (Math/pow 10 digits))]
     (loop [i 1
            result base]
       (if (>= i height)
         result
         (recur (inc i) (mod-expt base result modulus)))))))
