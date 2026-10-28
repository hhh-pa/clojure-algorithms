(ns clojure-algorithms.data-structures.binary-tree.fenwick-tree
  "Port of data_structures/binary_tree/fenwick_tree.py.
  0-indexed Fenwick (binary indexed) tree over immutable vectors.
  A tree is {:size n :tree [...]}.")

(defn make-fenwick
  "Build a Fenwick tree from ARR."
  [arr]
  (let [v (vec arr)
        size (count v)]
    {:size size
     :tree (loop [t v i 1]
             (if (>= i size)
               t
               (let [j (+ i (bit-and i (- i)))]
                 (recur (if (< j size) (update t j + (nth t i)) t)
                        (inc i)))))}))

(defn make-fenwick-sized
  "Empty Fenwick tree with SIZE slots."
  [size]
  {:size size :tree (vec (repeat size 0))})

(defn next-index
  "Next responsible index after INDEX."
  [index]
  (+ index (bit-and index (- index))))

(defn prev-index
  "Previous responsible index before INDEX."
  [index]
  (- index (bit-and index (- index))))

(defn fenwick-add
  "Add VALUE at INDEX; returns the new tree."
  [{:keys [size tree] :as ft} index value]
  (if (zero? index)
    (assoc ft :tree (update tree 0 + value))
    (assoc ft :tree
           (loop [t tree i index]
             (if (>= i size) t
                 (recur (update t i + value) (next-index i)))))))

(defn fenwick-prefix
  "Sum of elements in [0, RIGHT)."
  [{:keys [tree]} right]
  (if (zero? right)
    0
    (loop [result (nth tree 0) r (dec right)]
      (if (pos? r)
        (recur (+ result (nth tree r)) (prev-index r))
        result))))

(defn fenwick-query
  "Sum of elements in [LEFT, RIGHT)."
  [ft left right]
  (- (fenwick-prefix ft right) (fenwick-prefix ft left)))

(defn fenwick-get
  "Element at INDEX."
  [ft index]
  (fenwick-query ft index (inc index)))

(defn fenwick-update
  "Set INDEX to VALUE; returns the new tree."
  [ft index value]
  (fenwick-add ft index (- value (fenwick-get ft index))))

(defn fenwick-array
  "Plain array backing the tree."
  [{:keys [size tree]}]
  (loop [arr tree i (dec size)]
    (if (<= i 0)
      arr
      (let [j (next-index i)]
        (recur (if (< j size) (update arr j - (nth arr i)) arr)
               (dec i))))))

(defn rank-query
  "Largest index with prefix sum <= VALUE (-1 when none).
  Requires non-negative values."
  [{:keys [size tree]} value]
  (let [value (- value (nth tree 0))]
    (if (neg? value)
      -1
      (let [j (loop [j 1] (if (< (* j 2) size) (recur (* j 2)) j))]
        (loop [i 0 v value j j]
          (if (zero? j)
            i
            (if (and (< (+ i j) size) (<= (nth tree (+ i j)) v))
              (recur (+ i j) (- v (nth tree (+ i j))) (quot j 2))
              (recur i v (quot j 2)))))))))
