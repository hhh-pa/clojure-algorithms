(ns clojure-algorithms.maths.prime-sieve-eratosthenes
  "Sieve of Eratosthenes

Input: n = 10
Output: 2 3 5 7

Input: n = 20
Output: 2 3 5 7 11 13 17 19

you can read in detail about this at
https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes")

(defn prime-sieve-eratosthenes
  "Return the prime numbers up to n (inclusive) as a vector."
  [num]
  (when (<= num 0)
    (throw (ex-info "Input must be a positive integer" {:type :ValueError})))
  (let [sieve (boolean-array (inc num) true)]
    (loop [p 2]
      (when (<= (*' p p) num)
        (when (aget sieve p)
          (loop [i (* p p)]
            (when (<= i num)
              (aset sieve i false)
              (recur (+ i p)))))
        (recur (inc p))))
    (filterv #(aget sieve %) (range 2 (inc num)))))
