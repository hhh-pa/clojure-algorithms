(ns clojure-algorithms.maths.next-prime-number
  "Next Prime Number -- https://en.wikipedia.org/wiki/Prime_number

Find the smallest prime number strictly greater than a given number.")

(defn is-prime
  "Check if a number is prime."
  [number]
  (cond
    (<= number 1) false
    (<= number 3) true
    (or (zero? (mod number 2)) (zero? (mod number 3))) false
    :else (loop [factor 5]
            (cond
              (> (* factor factor) number) true
              (or (zero? (mod number factor))
                  (zero? (mod number (+ factor 2)))) false
              :else (recur (+ factor 6))))))

(defn next-prime
  "Find the smallest prime number strictly greater than the given number."
  [number]
  (when-not (integer? number)
    (throw (ex-info "next_prime() only accepts integral values"
                    {:type :ValueError})))
  (loop [candidate (inc number)]
    (if (is-prime candidate)
      candidate
      (recur (inc candidate)))))
