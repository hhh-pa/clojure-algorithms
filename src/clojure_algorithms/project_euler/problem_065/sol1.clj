(ns clojure-algorithms.project-euler.problem-065.sol1
  "Project Euler 65: convergents of e.

  The sum of digits in the numerator of the 10th convergent of the
  continued fraction for e is 1 + 4 + 5 + 7 = 17. Find the sum of the
  digits in the numerator of the 100th convergent of the continued
  fraction for e.

  The i-th numerator follows n_i = m_i * n_(i-1) + n_(i-2), where m_i
  is the i-th index of the continued fraction representation of e.")

(defn sum-digits
  "Returns the sum of every digit in num."
  [num]
  (loop [n num digit-sum 0]
    (if (zero? n)
      digit-sum
      (recur (quot n 10) (+ digit-sum (rem n 10))))))

(defn solution
  "Returns the sum of the digits in the numerator of the max-n-th
  convergent of the continued fraction for e."
  ([] (solution 100))
  ([max-n]
   (loop [i 2 pre-numerator 1 cur-numerator 2]
     (if (> i max-n)
       (sum-digits cur-numerator)
       (let [e-cont (if (zero? (mod i 3)) (quot (* 2 i) 3) 1)]
         (recur (inc i)
                cur-numerator
                (+' (*' e-cont cur-numerator) pre-numerator)))))))
