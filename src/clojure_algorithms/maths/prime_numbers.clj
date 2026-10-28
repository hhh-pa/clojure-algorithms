(ns clojure-algorithms.maths.prime-numbers
  "Port of maths/prime_numbers.py")

(defn- prime-slow?
  "Trial division by every integer below `n`."
  [n]
  (not-any? #(zero? (rem n %)) (range 2 n)))

(defn- prime?
  "Trial division by integers up to sqrt(n)."
  [n]
  (let [bound (inc (long (Math/sqrt n)))]
    (not-any? #(zero? (rem n %)) (range 2 bound))))

(defn- prime-odd?
  "Trial division of the odd `n` by odd integers up to sqrt(n)."
  [n]
  (let [bound (inc (long (Math/sqrt n)))]
    (not-any? #(zero? (rem n %)) (range 3 bound 2))))

(defn slow-primes
  "Returns a vector of all prime numbers up to `max-n` (slow version)."
  [max-n]
  (filterv prime-slow? (filter #(> % 1) (range 1 (inc max-n)))))

(defn primes
  "Returns a vector of all prime numbers up to `max-n`."
  [max-n]
  (filterv prime? (filter #(> % 1) (range 1 (inc max-n)))))

(defn fast-primes
  "Returns a vector of all prime numbers up to `max-n`, skipping even
  candidates."
  [max-n]
  (let [odds (filterv prime-odd?
                      (filter #(> % 1) (range 1 (inc max-n) 2)))]
    (if (> max-n 2) (vec (cons 2 odds)) odds)))

(defn benchmark
  "Upstream only times generator creation with the timeit harness, which has
  no Clojure equivalent; kept as a stub so the public var still exists."
  [& _args]
  (throw (ex-info "benchmark requires the timeit harness" {:type :UnsupportedOperation})))
