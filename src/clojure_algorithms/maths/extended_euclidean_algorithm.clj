(ns clojure-algorithms.maths.extended-euclidean-algorithm
  "Extended Euclidean Algorithm.
Finds 2 numbers a and b such that it satisfies
the equation am + bn = gcd(m, n) (a.k.a Bezout's Identity).
https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm")

(defn- floordiv
  "Floor division, matching Python's // operator."
  [a b]
  (let [q (quot a b)]
    (if (and (not (zero? (rem a b))) (not= (neg? a) (neg? b)))
      (dec q)
      q)))

(defn extended-euclidean-algorithm
  "Extended Euclidean Algorithm. Finds [x y] such that
x * a + y * b = gcd(a, b) (Bezout's Identity)."
  [a b]
  (cond
    (= (abs a) 1) [a 0]
    (= (abs b) 1) [0 b]
    :else
    (loop [old-r a, r b, old-x 1, x 0, old-y 0, y 1]
      (if (zero? r)
        [(if (neg? a) (-' old-x) old-x)
         (if (neg? b) (-' old-y) old-y)]
        (let [q (floordiv old-r r)]
          (recur r (-' old-r (*' q r))
                 x (-' old-x (*' q x))
                 y (-' old-y (*' q y))))))))

(defn main
  "Call Extended Euclidean Algorithm."
  []
  (let [args *command-line-args*]
    (if (< (count args) 2)
      (do (println "2 integer arguments required") 1)
      (let [a (Long/parseLong (nth args 0))
            b (Long/parseLong (nth args 1))]
        (println (extended-euclidean-algorithm a b))
        0))))
