(ns clojure-algorithms.maths.hardy-ramanujanalgo
  "This theorem states that the number of prime factors of n
will be approximately log(log(n)) for most natural numbers n")

(defn- strip-factor
  "Divide m by p until it no longer divides m."
  [m p]
  (loop [m m]
    (if (zero? (rem m p))
      (recur (quot m p))
      m)))

(defn exact-prime-factor-count
  "Count the distinct prime factors of the positive integer n."
  [n]
  (when (or (not (number? n)) (< n 1))
    (throw (ex-info "n must be a positive integer" {:type :ValueError})))
  (let [[m c] (if (even? n) [(strip-factor n 2) 1] [n 0])
        [m c] (loop [m m
                     i 3
                     c c]
                (if (> i (long (Math/sqrt (double m))))
                  [m c]
                  (if (zero? (rem m i))
                    (recur (strip-factor m i) (+ i 2) (inc c))
                    (recur m (+ i 2) c))))]
    (if (> m 2) (inc c) c)))
