(ns clojure-algorithms.maths.karatsuba
  "Multiply two numbers using Karatsuba algorithm.")

(defn karatsuba
  "Multiply a and b with the Karatsuba algorithm."
  [a b]
  (if (or (= 1 (count (str a))) (= 1 (count (str b))))
    (* a b)
    (let [m1 (max (count (str a)) (count (str b)))
          m2 (quot m1 2)
          base (reduce *' 1 (repeat m2 10))
          a1 (quot a base)
          a2 (rem a base)
          b1 (quot b base)
          b2 (rem b base)
          x (karatsuba a2 b2)
          y (karatsuba (+ a1 a2) (+ b1 b2))
          z (karatsuba a1 b1)]
      (+ (* z base base) (* (- y z x) base) x))))

(defn main
  []
  (println (karatsuba 15463 23489))
  nil)
