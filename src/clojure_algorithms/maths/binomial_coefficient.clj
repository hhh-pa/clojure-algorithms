(ns clojure-algorithms.maths.binomial-coefficient
  "Binomial coefficient via Pascal's triangle.")

(defn binomial-coefficient
  "Calculate C(n, r) using Pascal's triangle."
  [n r]
  (when (or (and (number? n) (neg? n)) (and (number? r) (neg? r)))
    (throw (ex-info "n and r must be non-negative integers" {:type :ValueError})))
  (when (or (not (integer? n)) (not (integer? r)))
    (throw (ex-info "'float' object cannot be interpreted as an integer"
                    {:type :TypeError})))
  (if (or (zero? n) (zero? r))
    1
    (let [final-row (loop [i 1
                           c (assoc (vec (repeat (inc r) 0)) 0 1)]
                      (if (> i n)
                        c
                        (recur (inc i)
                               (loop [j (min i r) c c]
                                 (if (<= j 0)
                                   c
                                   (recur (dec j)
                                          (assoc c j (+ (nth c j) (nth c (dec j))))))))))]
      (nth final-row r))))
