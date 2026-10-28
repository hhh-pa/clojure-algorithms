(ns clojure-algorithms.maths.eulers-totient
  "Eulers Totient function finds the number of relative primes of a
  number n from 1 to n.")

(defn totient
  "Returns a vector `t` of length n+1 where `t[i]` is the number of
  relative primes of i (Euler's totient), computed with a linear sieve."
  [n]
  (loop [i 2
         is-prime (vec (repeat (inc n) true))
         totients (vec (map dec (range (inc n))))
         primes []]
    (if (> i n)
      totients
      (let [primes (if (is-prime i) (conj primes i) primes)
            [is-prime totients]
            (loop [js primes
                   is-prime is-prime
                   totients totients]
              (if (empty? js)
                [is-prime totients]
                (let [p (first js)
                      ip (* i p)]
                  (if (>= ip n)
                    [is-prime totients]
                    (let [is-prime (assoc is-prime ip false)]
                      (if (zero? (mod i p))
                        [is-prime (assoc totients ip (* (totients i) p))]
                        (recur (rest js)
                               is-prime
                               (assoc totients ip
                                      (* (totients i) (dec p))))))))))]
        (recur (inc i) is-prime totients primes)))))
