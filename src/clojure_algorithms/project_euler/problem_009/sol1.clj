(ns clojure-algorithms.project-euler.problem-009.sol1
  "Project Euler Problem 9: special Pythagorean triplet.

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for
  which a^2 + b^2 = c^2. For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.")

(defn solution
  "Returns the product of a, b, c which form a Pythagorean triplet with
  a < b < c, a^2 + b^2 = c^2 and a + b + c = 1000."
  []
  (loop [a 0]
    (if (>= a 300)
      -1
      (let [found (loop [b (inc a)]
                    (if (>= b 400)
                      nil
                      (let [found (loop [c (inc b)]
                                    (cond (>= c 500) nil
                                          (and (= 1000 (+ a b c))
                                               (= (* c c) (+ (* a a) (* b b))))
                                          (* a b c)
                                          :else (recur (inc c))))]
                        (if found found (recur (inc b))))))]
        (if found found (recur (inc a)))))))

(defn solution-fast
  "Returns the product of a, b, c which form a Pythagorean triplet with
  a < b < c, a^2 + b^2 = c^2 and a + b + c = 1000.

  Faster version that derives c from a and b instead of looping over it."
  []
  (loop [a 0]
    (if (>= a 300)
      -1
      (let [found (loop [b 0]
                    (if (>= b 400)
                      nil
                      (let [c (- 1000 a b)]
                        (if (and (< a b c) (= (* c c) (+ (* a a) (* b b))))
                          (* a b c)
                          (recur (inc b))))))]
        (if found found (recur (inc a)))))))

(defn benchmark
  "Prints the time taken by solution and solution-fast for a single call
  each, in milliseconds."
  []
  (let [start (System/nanoTime)
        _ (solution)
        mid (System/nanoTime)
        _ (solution-fast)
        end (System/nanoTime)]
    (println "solution:" (/ (double (- mid start)) 1e6) "ms")
    (println "solution-fast:" (/ (double (- end mid)) 1e6) "ms")))
