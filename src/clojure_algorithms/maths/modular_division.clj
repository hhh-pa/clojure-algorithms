(ns clojure-algorithms.maths.modular-division
  "Port of maths/modular_division.py.")

(defn greatest-common-divisor
  "Euclid's Lemma: d divides a and b, if and only if d divides a-b and b.
  Euclid's Algorithm."
  [a b]
  (loop [a a b b]
    (let [[a b] (if (< a b) [b a] [a b])]
      (if (zero? (mod a b))
        b
        (recur b (mod a b))))))

(defn extended-gcd
  "Extended Euclid's Algorithm: if d divides a and b and
  d = a*x + b*y for integers x and y, then d = gcd(a,b).
  Returns [d x y]."
  [a b]
  (assert (>= a 0))
  (assert (>= b 0))
  (let [[d x y] (if (zero? b)
                  [a 1 0]
                  (let [[d p q] (extended-gcd b (mod a b))]
                    [d q (- p (* q (quot a b)))]))]
    (assert (zero? (mod a d)))
    (assert (zero? (mod b d)))
    (assert (= d (+ (* a x) (* b y))))
    [d x y]))

(defn extended-euclid
  "Extended Euclid. Returns [x y] such that a*x + b*y = gcd(a,b)."
  [a b]
  (if (zero? b)
    [1 0]
    (let [[x y] (extended-euclid b (mod a b))
          k (quot a b)]
      [y (- x (* k y))])))

(defn invert-modulo
  "Finds the inverse of a, i.e. a^(-1), modulo n."
  [a n]
  (let [[b _x] (extended-euclid a n)]
    (if (neg? b)
      (mod (+ (mod b n) n) n)
      b)))

(defn modular-division
  "Modular Division: an efficient algorithm for dividing b by a modulo n.

  Given three integers a, b, and n, such that gcd(a,n)=1 and n>1, returns
  an integer x such that 0<=x<=n-1 and b/a=x(mod n) (that is, b=ax(mod n)).

  Theorem: a has a multiplicative inverse modulo n iff gcd(a,n) = 1."
  [a b n]
  (when (<= n 1)
    (throw (ex-info "Modulus n must be greater than 1" {:type :ValueError})))
  (when (<= a 0)
    (throw (ex-info "Divisor a must be a positive integer" {:type :ValueError})))
  (when (not= (greatest-common-divisor a n) 1)
    (throw (ex-info "a and n must be coprime (gcd(a, n) = 1)"
                    {:type :ValueError})))
  (let [[_d _t s] (extended-gcd n a)]
    (mod (* b s) n)))

(defn modular-division2
  "Uses the inversion of a to find x = (b*a^(-1)) mod n."
  [a b n]
  (mod (* b (invert-modulo a n)) n))
