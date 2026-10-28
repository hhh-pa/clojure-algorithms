(ns clojure-algorithms.blockchain.diophantine-equation
  "Linear Diophantine equations a*x + b*y = c via the extended Euclidean
  algorithm. A solution exists iff gcd(a, b) divides c.")

(defn greatest-common-divisor
  "Returns gcd(|a|, |b|) via Euclid's algorithm."
  [a b]
  (loop [x (Math/abs (long a)), y (Math/abs (long b))]
    (if (zero? y) x (recur y (mod x y)))))

(defn extended-gcd
  "Extended Euclid's algorithm. Returns [d x y] with d = gcd(a, b) and
  d = a*x + b*y. Both inputs must be non-negative."
  [a b]
  (when (or (neg? a) (neg? b))
    (throw (ex-info "inputs must be non-negative" {:a a :b b})))
  (if (zero? b)
    [a 1 0]
    (let [[d p q] (extended-gcd b (mod a b))]
      [d q (- p (* q (quot a b)))])))

(defn diophantine
  "Returns one floating-point solution [x y] to a*x + b*y = c.
  Throws when gcd(a, b) does not divide c."
  [a b c]
  (when (not (zero? (mod c (greatest-common-divisor a b))))
    (throw (ex-info "no solution: gcd(a, b) does not divide c" {:a a :b b :c c})))
  (let [[d x y] (extended-gcd a b)
        r (/ (double c) d)]
    [(* r x) (* r y)]))

(defn diophantine-all-solutions
  "Returns the first `n` floating-point solutions [x y] to a*x + b*y = c
  (default 2), stepping along the general solution. Unlike the Python
  reference, which prints them, this returns a vector."
  ([a b c] (diophantine-all-solutions a b c 2))
  ([a b c n]
   (let [[x0 y0] (diophantine a b c)
         d (greatest-common-divisor a b)
         p (quot a d)
         q (quot b d)]
     (mapv (fn [i] [(+ x0 (* i q)) (- y0 (* i p))]) (range n)))))

(defn all-diophantine-solutions
  "Returns up to `n` integer solutions [x y] to a*x + b*y = c.
  Throws when no integer solution exists."
  ([a b c] (all-diophantine-solutions a b c 2))
  ([a b c n]
   (if (and (zero? a) (zero? b))
     (if (zero? c)
       (vec (take (min 1 n) [[0 0]]))
       (throw (ex-info "No integer solutions exist for a=0, b=0, c!=0" {})))
     (let [[g xg yg] (extended-gcd (Math/abs (long a)) (Math/abs (long b)))]
       (when (not (zero? (mod c g)))
         (throw (ex-info (format "No integer solutions exist for a=%s, b=%s, c=%s" a b c) {})))
       (let [scale (quot c g)
             x0 (if (neg? a) (- (* xg scale)) (* xg scale))
             y0 (if (neg? b) (- (* yg scale)) (* yg scale))
             dx (quot b g)
             dy (quot a g)]
         (mapv (fn [t] [(+ x0 (* t dx)) (- y0 (* t dy))]) (range n)))))))
