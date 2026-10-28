(ns clojure-algorithms.maths.ncr-combinations
  "Generalized combinations (n choose r) calculator for real total and integer choose.
  Wikipedia URL:
  https://en.wikipedia.org/wiki/Binomial_coefficient")

(defn combinations
  "Computes the number of combinations: total * (total-1) * ... *
  (total-choose+1) / choose!. `total` may be any real number; `choose`
  must be a non-negative integer."
  [total choose]
  (when (or (not (integer? choose)) (neg? choose))
    (throw (ex-info "choose must be a non-negative integer" {:type :ValueError})))
  (if (zero? choose)
    1.0
    (let [numerator (reduce * 1.0 (map #(- total %) (range choose)))
          denominator (reduce *' 1 (range 1 (inc choose)))]
      (/ numerator denominator))))
