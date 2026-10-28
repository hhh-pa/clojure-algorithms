(ns clojure-algorithms.data-structures.arrays.product-sum
  "Port of data_structures/arrays/product_sum.py.
  Product sum of nested arrays (recursive and iterative).
  The timing benchmark() demo is omitted.")

(defn product-sum
  "Product sum of ARR where nested levels multiply by DEPTH."
  [arr depth]
  (* depth
     (reduce + (map (fn [x]
                      (if (sequential? x)
                        (product-sum x (inc depth))
                        x))
                    arr))))

(defn product-sum-array
  "Product sum of ARR at depth 1."
  [arr]
  (product-sum arr 1))

(defn product-sum-iterative
  "Product sum of ARR via breadth-first traversal."
  [arr]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [arr 1 1])
         total 0]
    (if (empty? queue)
      total
      (let [[lst depth mult] (peek queue)
            queue (pop queue)
            [total queue]
            (reduce (fn [[t q] x]
                      (if (sequential? x)
                        [t (conj q [x (inc depth) (* mult (inc depth))])]
                        [(+ t (* x mult)) q]))
                    [total queue] lst)]
        (recur queue total)))))
