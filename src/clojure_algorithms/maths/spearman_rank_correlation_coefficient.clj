(ns clojure-algorithms.maths.spearman-rank-correlation-coefficient
  "Spearman's rank correlation coefficient."
  (:import [java.math BigDecimal RoundingMode]))

(defn assign-ranks
  "Assigns ranks to elements in data; tied values share the average rank."
  [data]
  (let [n (count data)
        ranked (vec (sort-by first (map-indexed (fn [idx v] [v idx]) data)))]
    (loop [i 0 ranks (vec (repeat n 0.0))]
      (if (>= i n)
        ranks
        (let [j (loop [j i]
                  (if (and (< j (dec n))
                           (== (first (ranked (inc j))) (first (ranked i))))
                    (recur (inc j))
                    j))
              avg-rank (+ (/ (+ i j) 2.0) 1)]
          (recur (inc j)
                 (reduce (fn [r k] (assoc r (second (ranked k)) avg-rank))
                         ranks
                         (range i (inc j)))))))))

(defn- round1
  "Round to one decimal with HALF_EVEN, matching Python's round(x, 1)."
  [x]
  (.doubleValue (.setScale (BigDecimal/valueOf (double x)) 1 RoundingMode/HALF_EVEN)))

(defn calculate-spearman-rank-correlation
  "Calculates Spearman's rank correlation coefficient of two variables."
  [variable-1 variable-2]
  (let [n (count variable-1)]
    (when (< n 2)
      (throw (ex-info "Need at least 2 data points to calculate correlation"
                      {:type :ValueError})))
    (let [rank-var1 (assign-ranks variable-1)
          rank-var2 (assign-ranks variable-2)
          d-squared (reduce + 0.0
                            (map (fn [rx ry]
                                   (let [d (- rx ry)]
                                     (* d d)))
                                 rank-var1 rank-var2))
          rho (- 1 (/ (* 6 d-squared) (* n (dec (* n n)))))]
      (round1 rho))))
