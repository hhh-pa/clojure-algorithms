(ns clojure-algorithms.maths.tonelli-shanks
  "Tonelli-Shanks algorithm for modular square roots.

Given an odd prime modulus prime and an integer residue, find an
integer root such that root^2 = residue (mod prime), or report that
no square root exists.

https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm")

(defn- mod-pow
  "Binary modular exponentiation: (base ** exp) % m."
  [base exp m]
  (loop [b (mod base m), e exp, r 1]
    (if (zero? e)
      r
      (recur (mod (*' b b) m)
             (quot e 2)
             (if (odd? e) (mod (*' r b) m) r)))))

(defn- check-odd-prime
  "Validates the prime modulus for the Tonelli-Shanks routines."
  [prime]
  (when (or (<= prime 2) (even? prime))
    (throw (ex-info "prime must be an odd prime" {:type :ValueError}))))

(defn legendre-symbol
  "Compute the Legendre symbol (residue / prime). Returns 1 if residue is
a quadratic residue modulo prime (and not divisible by prime), -1 if it
is a non-residue, and 0 if residue = 0 (mod prime)."
  [residue prime]
  (check-odd-prime prime)
  (let [symbol (mod-pow (mod residue prime) (quot (dec prime) 2) prime)]
    (if (= symbol (dec prime)) -1 symbol)))

(defn- least-i
  "Finds the least i in [1, s) with t^(2^i) = 1 (mod prime), or nil."
  [t s prime]
  (loop [test-power t, i 1]
    (cond
      (>= i s) nil
      (= (mod-pow test-power 2 prime) 1) i
      :else (recur (mod-pow test-power 2 prime) (inc i)))))

(defn tonelli-shanks
  "Return a modular square root of residue modulo the odd prime prime.
When both roots exist, the smaller non-negative representative is returned.
Raises an error when residue is not a quadratic residue, or when prime
is not a valid odd prime modulus for this routine."
  [residue prime]
  (check-odd-prime prime)
  (let [residue (mod residue prime)]
    (if (zero? residue)
      0
      (do
        (when (not= (legendre-symbol residue prime) 1)
          (throw (ex-info (str residue " is not a quadratic residue modulo "
                               prime)
                          {:type :ValueError})))
        (if (= (mod prime 4) 3)
          (let [root (mod-pow residue (quot (inc prime) 4) prime)]
            (min root (- prime root)))
          (let [[q s] (loop [q (dec prime), s 0]
                        (if (even? q)
                          (recur (quot q 2) (inc s))
                          [q s]))
                z (first (filter #(= (legendre-symbol % prime) -1)
                                 (iterate inc 2)))]
            (loop [c (mod-pow z q prime)
                   r (mod-pow residue (quot (inc q) 2) prime)
                   t (mod-pow residue q prime)
                   s s]
              (if (= t 1)
                (min r (- prime r))
                (let [i (least-i t s prime)]
                  (when (nil? i)
                    (throw (ex-info (str residue
                                         " is not a quadratic residue modulo "
                                         prime)
                                    {:type :ValueError})))
                  (let [b (mod-pow c (bit-shift-left 1 (- s i 1)) prime)
                        b2 (mod (*' b b) prime)]
                    (recur b2
                           (mod (*' r b) prime)
                           (mod (*' t b2) prime)
                           i)))))))))))
