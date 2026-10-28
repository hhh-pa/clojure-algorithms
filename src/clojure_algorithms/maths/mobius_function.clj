(ns clojure-algorithms.maths.mobius-function
  "Mobius function.
References: https://en.wikipedia.org/wiki/M%C3%B6bius_function
References: wikipedia:square free number")

(defn- python-type-name
  "Python type name used in comparison TypeError messages."
  [v]
  (cond
    (nil? v) "NoneType"
    (string? v) "str"
    (boolean? v) "bool"
    (or (sequential? v) (map? v) (set? v)) "list"
    :else "object"))

(defn- prime-factors
  "Prime factors of n as a vector, mirroring maths/prime_factors.py:
trial division while i*i <= n, then n itself if > 1."
  [n]
  (when-not (number? n)
    (throw (ex-info (str "'<=' not supported between instances of 'int' and '"
                         (python-type-name n) "'")
                    {:type :TypeError})))
  (loop [n n i 2 factors []]
    (if (<= (*' i i) n)
      (if (zero? (mod n i))
        (recur (quot n i) i (conj factors i))
        (recur n (inc i) factors))
      (if (> n 1) (conj factors n) factors))))

(defn- square-free?
  "True when no factor repeats (numeric == comparison, as in Python sets)."
  [factors]
  (loop [seen [] remaining factors]
    (cond
      (empty? remaining) true
      (some #(== % (first remaining)) seen) false
      :else (recur (conj seen (first remaining)) (rest remaining)))))

(defn mobius
  "Mobius function: 0 when n is not square free, else -1 for an odd
number of prime factors and 1 for an even number."
  [n]
  (let [factors (prime-factors n)]
    (if (square-free? factors)
      (if (odd? (count factors)) -1 1)
      0)))
