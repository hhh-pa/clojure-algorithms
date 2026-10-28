(ns clojure-algorithms.maths.sieve-of-atkin
  "Sieve of Atkin algorithm for finding all prime numbers up to a given limit.

The Sieve of Atkin is a modern variant of the ancient Sieve of Eratosthenes
that is optimized for finding primes. It has better theoretical asymptotic
complexity, especially for large ranges. This is the basic, non-segmented
form.

Time Complexity: O(n / log log n)
Space Complexity: O(n)

Reference: https://en.wikipedia.org/wiki/Sieve_of_Atkin")

(defn sieve-of-atkin
  "Generate all prime numbers up to limit (inclusive) using the Sieve of Atkin."
  [limit]
  (when (neg? limit)
    (throw (ex-info (str limit ": Invalid input, please enter a non-negative integer.")
                    {:type :ValueError})))
  (if (< limit 2)
    []
    (let [sieve (boolean-array (inc limit) false)
          sqrt-limit (inc (long (Math/sqrt limit)))]
      (aset sieve 2 true)
      (when (>= limit 3)
        (aset sieve 3 true))
      (doseq [x (range 1 sqrt-limit)
              y (range 1 sqrt-limit)]
        (let [n (+ (* 4 x x) (* y y))]
          (when (and (<= n limit) (or (= (mod n 12) 1) (= (mod n 12) 5)))
            (aset sieve n (not (aget sieve n)))))
        (let [n (+ (* 3 x x) (* y y))]
          (when (and (<= n limit) (= (mod n 12) 7))
            (aset sieve n (not (aget sieve n)))))
        (when (> x y)
          (let [n (- (* 3 x x) (* y y))]
            (when (and (<= n limit) (= (mod n 12) 11))
              (aset sieve n (not (aget sieve n)))))))
      (doseq [r (range 5 sqrt-limit)
              :when (aget sieve r)]
        (let [square (* r r)]
          (loop [i square]
            (when (<= i limit)
              (aset sieve i false)
              (recur (+ i square))))))
      (filterv #(aget sieve %) (range 2 (inc limit))))))
