(ns clojure-algorithms.project-euler.problem-174.sol1
  "Project Euler Problem 174: lamina type counts.

  A square lamina is a square outline with a square hole. If t is the number
  of tiles used, t = 8 is type L(1) and t = 32 is type L(2). Let N(n) be the
  number of t <= 1000000 such that t is type L(n); for example N(15) = 832.
  Computes the sum of N(n) for 1 <= n <= 10.")

(defn solution
  "Returns the sum of N(n) for 1 <= n <= n-limit with t up to t-limit."
  ([] (solution 1000000 10))
  ([t-limit] (solution t-limit 10))
  ([t-limit n-limit]
   (let [counts (reduce (fn [acc outer-width]
                          (let [hole-low (if (> (* outer-width outer-width)
                                                t-limit)
                                           (max (long (Math/ceil
                                                       (Math/sqrt
                                                        (- (* outer-width
                                                              outer-width)
                                                           t-limit))))
                                                1)
                                           1)
                                hole-start (+ hole-low
                                              (mod (- outer-width hole-low) 2))]
                            (reduce (fn [a hole-width]
                                      (assoc! a
                                              (- (* outer-width outer-width)
                                                 (* hole-width hole-width))
                                              (inc (get a
                                                        (- (* outer-width
                                                              outer-width)
                                                           (* hole-width
                                                              hole-width))
                                                        0))))
                                    acc
                                    (range hole-start (dec outer-width) 2))))
                        (transient {})
                        (range 3 (+ (quot t-limit 4) 2)))]
     (count (filter #(<= 1 % n-limit) (vals (persistent! counts)))))))
