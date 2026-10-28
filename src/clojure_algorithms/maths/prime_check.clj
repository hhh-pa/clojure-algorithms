(ns clojure-algorithms.maths.prime-check
  "Prime Check.")

(defn is-prime
  "Checks to see if a number is a prime in O(sqrt(n))."
  [number]
  (when (or (not (integer? number)) (neg? number))
    (throw (ex-info "is_prime() only accepts positive integers"
                    {:type :ValueError})))
  (cond (and (< 1 number) (< number 4)) true
        (or (< number 2)
            (zero? (mod number 2))
            (zero? (mod number 3))) false
        :else (let [limit (int (inc (Math/sqrt number)))]
                (loop [i 5]
                  (cond (>= i limit) true
                        (or (zero? (mod number i))
                            (zero? (mod number (+ i 2)))) false
                        :else (recur (+ i 6)))))))
