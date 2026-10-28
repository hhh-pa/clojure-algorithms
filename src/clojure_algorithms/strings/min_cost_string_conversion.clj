(ns clojure-algorithms.strings.min-cost-string-conversion
  "Cost-optimal string conversion tables (copy/replace/delete/insert).
  Ported from TheAlgorithms/Python.")

(defn compute-transform-tables
  "Builds [costs ops] tables for converting `source` into `dest` with the
  given operation costs. Ops entries are strings like \"Cc\", \"Rau\",
  \"Dt\", \"Ic\"."
  [source dest copy-cost replace-cost delete-cost insert-cost]
  (let [src (vec source)
        dst (vec dest)
        ns (count src)
        nd (count dst)
        costs (vec (for [i (range (inc ns))]
                     (vec (for [j (range (inc nd))]
                            (cond (and (zero? i) (zero? j)) 0
                                  (zero? i) (* j insert-cost)
                                  (zero? j) (* i delete-cost)
                                  :else 0)))))
        ops (vec (for [i (range (inc ns))]
                   (vec (for [j (range (inc nd))]
                          (cond (and (zero? i) (zero? j)) "0"
                                (zero? i) (str "I" (nth dst (dec j)))
                                (zero? j) (str "D" (nth src (dec i)))
                                :else "0")))))]
    (loop [c costs o ops i 1]
      (if (> i ns)
        [c o]
        (let [[c2 o2]
              (loop [cc c oo o j 1]
                (if (> j nd)
                  [cc oo]
                  (let [same? (= (nth src (dec i)) (nth dst (dec j)))
                        [best op] (if same?
                                    [(+ (get-in cc [(dec i) (dec j)]) copy-cost)
                                     (str "C" (nth src (dec i)))]
                                    [(+ (get-in cc [(dec i) (dec j)])
                                        replace-cost)
                                     (str "R" (nth src (dec i))
                                          (nth dst (dec j)))])
                        [best2 op2] (if (< (+ (get-in cc [(dec i) j])
                                              delete-cost)
                                           best)
                                      [(+ (get-in cc [(dec i) j]) delete-cost)
                                       (str "D" (nth src (dec i)))]
                                      [best op])
                        [best3 op3] (if (< (+ (get-in cc [i (dec j)])
                                              insert-cost)
                                           best2)
                                      [(+ (get-in cc [i (dec j)]) insert-cost)
                                       (str "I" (nth dst (dec j)))]
                                      [best2 op2])]
                    (recur (assoc-in cc [i j] best3)
                           (assoc-in oo [i j] op3) (inc j)))))]
          (recur c2 o2 (inc i)))))))

(defn assemble-transformation
  "Backtracks the `ops` table from [i, j] into an operation sequence."
  [ops i j]
  (if (and (zero? i) (zero? j))
    []
    (let [op (get-in ops [i j])]
      (case (first op)
        (\C \R) (conj (assemble-transformation ops (dec i) (dec j)) op)
        \D (conj (assemble-transformation ops (dec i) j) op)
        (conj (assemble-transformation ops i (dec j)) op)))))
