(ns clojure-algorithms.dynamic-programming.matrix-chain-multiplication
  "Minimum scalar multiplications for a chain of matrices with
  dimensions `dims`, via bottom-up DP and memoized recursion.
  Ported from TheAlgorithms/Python
  dynamic_programming/matrix_chain_multiplication.py.")

(def ^:private max-cost Long/MAX_VALUE)

(defn matrix-chain-multiply
  "Minimum multiplications for the chain described by `dims` (bottom-up)."
  [dims]
  (let [arr (vec dims)
        n (count arr)]
    (if (< n 2)
      0
      (let [dp (atom (vec (repeat n (vec (repeat n max-cost)))))]
        (doseq [i (range (dec n) 0 -1)
                j (range i n)]
          (if (= i j)
            (swap! dp assoc-in [i j] 0)
            (swap! dp assoc-in [i j]
                   (reduce min (for [k (range i j)]
                                 (+ (get-in @dp [i k])
                                    (get-in @dp [(inc k) j])
                                    (* (arr (dec i)) (arr k) (arr j))))))))
        (get-in @dp [1 (dec n)])))))

(defn matrix-chain-order
  "Minimum multiplications for the chain described by `dims` (memoized)."
  [dims]
  (let [arr (vec dims)
        memo (atom {})]
    (letfn [(cost [i j]
              (if (contains? @memo [i j])
                (@memo [i j])
                (let [c (if (>= (inc i) j)
                          0
                          (reduce min (for [k (range (inc i) j)]
                                        (+ (cost i k)
                                           (* (arr i) (arr k) (arr j))
                                           (cost k j)))))]
                  (swap! memo assoc [i j] c)
                  c)))]
      (cost 0 (dec (count arr))))))
