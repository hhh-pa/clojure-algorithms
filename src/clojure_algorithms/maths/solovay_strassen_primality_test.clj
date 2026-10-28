(ns clojure-algorithms.maths.solovay-strassen-primality-test
  "This script implements the Solovay-Strassen Primality test.

  This probabilistic primality test is based on Euler's criterion. It is similar
  to the Fermat test but uses quadratic residues. It can quickly identify
  composite numbers but may occasionally classify composite numbers as prime.

  More details and concepts about this can be found on:
  https://en.wikipedia.org/wiki/Solovay%E2%80%93Strassen_primality_test")

(defn jacobi-symbol
  "Calculates the Jacobi symbol (random-a/number), used to determine whether
  an integer is a quadratic residue modulo another integer."
  [random-a number]
  (if (#{0 1} random-a)
    random-a
    (loop [a (mod random-a number), n number, t 1]
      (if (zero? a)
        (if (== n 1) t 0)
        (let [[a t] (loop [a a, t t]
                      (if (even? a)
                        (recur (quot a 2)
                               (if (#{3 5} (mod n 8)) (- t) t))
                        [a t]))
              [a n] [n a]
              t (if (and (== (mod a 4) 3) (== (mod n 4) 3)) (- t) t)]
          (recur (mod a n) n t))))))

(defn- powmod
  "Modular exponentiation: base^exp mod m."
  [base exp m]
  (loop [b (mod base m), e exp, acc 1]
    (if (zero? e)
      acc
      (recur (mod (* b b) m)
             (quot e 2)
             (if (odd? e) (mod (* acc b) m) acc)))))

(defn solovay-strassen
  "Returns true if `number` is probably prime according to the
  Solovay-Strassen test run for `iterations` rounds."
  [number iterations]
  (cond
    (<= number 1) false
    (<= number 3) true
    :else (loop [i 0]
            (if (>= i iterations)
              true
              (let [a (+ 2 (rand-int (- number 3)))
                    x (jacobi-symbol a number)
                    y (powmod a (quot (dec number) 2) number)]
                (if (or (zero? x) (not= y (mod x number)))
                  false
                  (recur (inc i))))))))
