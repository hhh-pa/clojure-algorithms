(ns clojure-algorithms.maths.sum-of-geometric-progression
  "Sum of n terms in a geometric progression.")

(defn sum-of-geometric-progression
  "Return the sum of num-of-terms terms of the geometric progression."
  [first-term common-ratio num-of-terms]
  (if (= common-ratio 1)
    (* num-of-terms first-term)
    (* (/ (double first-term) (- 1 (double common-ratio)))
       (- 1 (Math/pow (double common-ratio) (double num-of-terms))))))
