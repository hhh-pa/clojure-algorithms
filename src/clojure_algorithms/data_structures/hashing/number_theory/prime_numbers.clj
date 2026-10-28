(ns clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
  "Port of data_structures/hashing/number_theory/prime_numbers.py.")

(defn is-prime
  "Primality test in O(sqrt(n))."
  [number]
  (when-not (and (integer? number) (>= number 0))
    (throw (ex-info "'number' must been an int and positive"
                    {:type :assertion-error})))
  (cond
    (< 1 number 4) true
    (or (< number 2) (zero? (mod number 2))) false
    :else (let [limit (inc (int (Math/sqrt number)))]
            (not-any? #(zero? (mod number %)) (range 3 limit 2)))))

(defn next-prime
  "Smallest prime strictly greater than FACTOR * VALUE.
  Pass DESC true to search downward."
  ([value] (next-prime value 1 false))
  ([value factor] (next-prime value factor false))
  ([value factor desc]
   (let [start (* factor value)
         step (if desc -1 1)
         candidate (loop [v start]
                     (if (is-prime v) v (recur (+ v step))))]
     (if (= candidate start)
       (next-prime (+ start step) 1 desc)
       candidate))))
