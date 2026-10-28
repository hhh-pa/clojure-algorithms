(ns clojure-algorithms.maths.segmented-sieve
  "Segmented Sieve.")

(defn sieve
  "Return the primes up to n (inclusive) using the segmented sieve.
  Throws :ValueError when n is not a positive integer."
  [n]
  (when-not (number? n)
    (throw (ex-info (str "Number " n " must instead be a positive integer")
                    {:type :TypeError})))
  (when (or (not (integer? n)) (<= n 0))
    (throw (ex-info (str "Number " n " must instead be a positive integer")
                    {:type :ValueError})))
  (let [end (long (Math/sqrt (double n)))
        in-prime (loop [start 2
                        temp (vec (repeat (inc end) true))
                        acc []]
                   (if (> start end)
                     acc
                     (if (nth temp start)
                       (recur (inc start)
                              (reduce (fn [t i] (assoc t i false))
                                      temp
                                      (range (* start start) (inc end) start))
                              (conj acc start))
                       (recur (inc start) temp acc))))]
    (loop [low (inc end)
           high (min (* 2 end) n)
           primes (vec in-prime)]
      (if (> low n)
        primes
        (let [temp (reduce (fn [t each]
                             (let [t0 (* (long (Math/floor (/ (double low) each)))
                                         each)
                                   t0 (if (< t0 low) (+ t0 each) t0)]
                               (reduce (fn [t2 j] (assoc t2 (- j low) false))
                                       t
                                       (range t0 (inc high) each))))
                           (vec (repeat (inc (- high low)) true))
                           in-prime)
              primes (into primes
                           (comp (map-indexed vector)
                                 (filter (fn [[_ marked]] (true? marked)))
                                 (map (fn [[j _]] (+ j low))))
                           temp)]
          (recur (inc high) (min (+ high end) n) primes))))))
