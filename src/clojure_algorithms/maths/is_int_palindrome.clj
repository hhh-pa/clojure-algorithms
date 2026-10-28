(ns clojure-algorithms.maths.is-int-palindrome
  "Port of maths/is_int_palindrome.py.")

(defn is-int-palindrome
  "Returns whether `num` is a palindrome or not
  (see for reference https://en.wikipedia.org/wiki/Palindromic_number)."
  [num]
  (if (neg? num)
    false
    (loop [n num rev 0]
      (if (zero? n)
        (= num rev)
        (recur (quot n 10) (+ (* rev 10) (mod n 10)))))))
