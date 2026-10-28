(ns clojure-algorithms.maths.chinese-remainder-theorem
  "Chinese Remainder Theorem.")

(defn- floordiv
  "Integer division rounding toward negative infinity."
  [a b]
  (let [q (quot a b)
        r (rem a b)]
    (if (or (zero? r) (= (neg? a) (neg? b))) q (dec q))))

(defn extended-euclid
  "Extended Euclid algorithm: returns [x y] such that a*x + b*y = gcd(a, b)."
  [a b]
  (if (zero? b)
    [1 0]
    (let [[x y] (extended-euclid b (mod a b))
          k (floordiv a b)]
      [y (- x (* k y))])))

(defn chinese-remainder-theorem
  "Smallest n such that n = r1 (mod n1) and n = r2 (mod n2)."
  [n1 r1 n2 r2]
  (let [[x y] (extended-euclid n1 n2)
        m (* n1 n2)
        n (+ (* r2 x n1) (* r1 y n2))]
    (mod (+ (mod n m) m) m)))

(defn invert-modulo
  "Find the modular inverse of a, i.e. a^(-1) mod n."
  [a n]
  (let [[b _x] (extended-euclid a n)]
    (if (neg? b)
      (mod (+ (mod b n) n) n)
      b)))

(defn chinese-remainder-theorem2
  "Same as chinese-remainder-theorem but using invert-modulo."
  [n1 r1 n2 r2]
  (let [x (invert-modulo n1 n2)
        y (invert-modulo n2 n1)
        m (* n1 n2)
        n (+ (* r2 x n1) (* r1 y n2))]
    (mod (+ (mod n m) m) m)))
