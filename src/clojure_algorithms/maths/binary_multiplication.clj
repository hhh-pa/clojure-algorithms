(ns clojure-algorithms.maths.binary-multiplication
  "Binary Multiplication.
A method to find a*b in O(log b) time, also useful when (a*b)%c is required
for numbers beyond the computer's calculation limits.")

(defn binary-multiply
  "Multiply 'a' and 'b' using bitwise multiplication."
  [a b]
  (loop [a a, b b, res 0]
    (if (<= b 0)
      res
      (recur (+' a a) (quot b 2) (if (odd? b) (+' res a) res)))))

(defn binary-mod-multiply
  "Calculate (a * b) % modulus using binary multiplication and modular
arithmetic."
  [a b modulus]
  (loop [a a, b b, res 0]
    (if (<= b 0)
      res
      (recur (+' a a)
             (quot b 2)
             (if (odd? b)
               (mod (+' (mod res modulus) (mod a modulus)) modulus)
               res)))))
