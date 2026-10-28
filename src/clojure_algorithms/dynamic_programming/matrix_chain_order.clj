(ns clojure-algorithms.dynamic-programming.matrix-chain-order
  "Matrix-chain multiplication returning the cost and split tables,
  plus parenthesization rendering.
  Ported from TheAlgorithms/Python dynamic_programming/matrix_chain_order.py.")

(defn matrix-chain-order
  "Return [cost-table split-table] for dimensions `array`."
  [array]
  (let [arr (vec array)
        n (count arr)
        cost (atom (vec (repeat n (vec (repeat n 0)))))
        sol (atom (vec (repeat n (vec (repeat n 0)))))]
    (doseq [chain-length (range 2 n)
            a (range 1 (inc (- n chain-length)))]
      (let [b (+ a chain-length -1)]
        (swap! cost assoc-in [a b] Long/MAX_VALUE)
        (doseq [c (range a b)]
          (let [price (+ (get-in @cost [a c]) (get-in @cost [(inc c) b])
                         (* (arr (dec a)) (arr c) (arr b)))]
            (when (< price (get-in @cost [a b]))
              (swap! cost assoc-in [a b] price)
              (swap! sol assoc-in [a b] c))))))
    [@cost @sol]))

(defn optimal-solution
  "Parenthesization string, e.g. \"( A1 ( A2 A3 ) ) \", for the split table."
  [sol i j]
  (if (= i j)
    (str "A" i " ")
    (str "( " (optimal-solution sol i (get-in sol [i j]))
         (optimal-solution sol (inc (get-in sol [i j])) j) ") ")))

(defn print-optimal-solution
  "Print the parenthesization for split table `sol` between `i` and `j`."
  [sol i j]
  (print (optimal-solution sol i j)))
