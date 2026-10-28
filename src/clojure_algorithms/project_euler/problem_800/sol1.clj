(ns clojure-algorithms.project-euler.problem-800.sol1
  "Project Euler Problem 800: hybrid integers.

  An integer of the form p^q * q^p with distinct primes p and q is called a
  hybrid-integer. Defines C(n) as the number of hybrid-integers less than or
  equal to n; C(800) = 2 and C(800^800) = 10790. Finds C(800800^800800).")

(defn calculate-prime-numbers
  "Returns prime numbers below max-number."
  [max-number]
  (if (<= max-number 2)
    []
    (let [is-prime (boolean-array max-number true)]
      (doseq [i (range 2 (inc (long (Math/sqrt (dec max-number)))))]
        (when (aget is-prime i)
          (loop [j (* i i)]
            (when (< j max-number)
              (aset is-prime j false)
              (recur (+ j i))))))
      (vec (filter #(aget is-prime %) (range 2 max-number))))))

(defn solution
  "Returns the number of hybrid-integers less than or equal to
  base^degree."
  ([] (solution 800800 800800))
  ([base degree]
   (let [log2 (fn [x] (/ (Math/log x) (Math/log 2)))
         upper-bound (* degree (log2 base))
         prime-numbers (vec (calculate-prime-numbers (long upper-bound)))
         logs (mapv log2 prime-numbers)]
     (loop [left 0
            right (dec (count prime-numbers))
            cnt 0]
       (if (>= left right)
         cnt
         (let [right (loop [r right]
                       (if (> (+ (* (nth prime-numbers r)
                                    (nth logs left))
                                 (* (nth prime-numbers left)
                                    (nth logs r)))
                              upper-bound)
                         (recur (dec r))
                         r))]
           (recur (inc left) right (+ cnt (- right left)))))))))
