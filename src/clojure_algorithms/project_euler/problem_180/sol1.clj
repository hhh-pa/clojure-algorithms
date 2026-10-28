(ns clojure-algorithms.project-euler.problem-180.sol1
  "Project Euler Problem 180: golden triples.

  Calls (x, y, z) a golden triple of order k if x, y, z are rationals of the
  form a/b with 0 < a < b <= k and fn(x, y, z) = 0 for some integer n. Since
  fn(x, y, z) = (x + y + z)(x^n + y^n - z^n), Fermat's Last Theorem restricts
  n to {-2, -1, 1, 2}. Sums u + v where t = u/v is the sum of all distinct
  s(x, y, z) = x + y + z for golden triples of order 35.")

(defn- gcd
  "Greatest common divisor of two integers."
  [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn is-sq
  "Checks if number is a perfect square."
  [number]
  (let [sq (long (Math/sqrt number))]
    (= number (* sq sq))))

(defn add-three
  "Given the numerators and denominators of three fractions, returns the
  numerator and denominator of their sum in lowest form as a vector."
  [x-num x-den y-num y-den z-num z-den]
  (let [top (+ (* x-num y-den z-den)
               (* y-num x-den z-den)
               (* z-num x-den y-den))
        bottom (* x-den y-den z-den)
        hcf (gcd top bottom)]
    [(quot top hcf) (quot bottom hcf)]))

(defn- reduce-frac
  "Reduces a numerator/denominator pair to lowest form."
  [num den]
  (let [hcf (gcd num den)]
    [(quot num hcf) (quot den hcf)]))

(defn- candidate-sums
  "Returns the distinct-sum candidates for given x and y fractions across
  the valid exponents n in {1, 2, -1, -2}."
  [x-num x-den y-num y-den order]
  (let [raw [[(+ (* x-num y-den) (* x-den y-num)) (* x-den y-den) false]
             [(+ (* x-num x-num y-den y-den)
                 (* x-den x-den y-num y-num))
              (* x-den x-den y-den y-den)
              true]
             [(* x-num y-num)
              (+ (* x-den y-num) (* x-num y-den))
              false]
             [(* x-num x-num y-num y-num)
              (+ (* x-den x-den y-num y-num)
                 (* x-num x-num y-den y-den))
              true]]]
    (keep (fn [[z-num z-den needs-sqrt]]
            (when (or (not needs-sqrt)
                      (and (is-sq z-num) (is-sq z-den)))
              (let [[zn zd] (if needs-sqrt
                              [(long (Math/sqrt z-num))
                               (long (Math/sqrt z-den))]
                              [z-num z-den])
                    [rzn rzd] (reduce-frac zn zd)]
                (when (and (pos? rzn) (< rzn rzd) (<= rzd order))
                  (add-three x-num x-den y-num y-den rzn rzd)))))
          raw)))

(defn solution
  "Finds the sum of the numerator and denominator of the sum of all
  s(x, y, z) for golden triples (x, y, z) of the given order."
  ([] (solution 35))
  ([order]
   (let [unique-s (into #{}
                        (for [x-num (range 1 (inc order))
                              x-den (range (inc x-num) (inc order))
                              y-num (range 1 (inc order))
                              y-den (range (inc y-num) (inc order))
                              s (candidate-sums x-num x-den y-num y-den order)]
                          s))
         total (reduce + 0 (map (fn [[num den]] (/ num den)) unique-s))]
     (+ (numerator total) (denominator total)))))
