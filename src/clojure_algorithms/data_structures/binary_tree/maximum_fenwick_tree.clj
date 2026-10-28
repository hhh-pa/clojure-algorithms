(ns clojure-algorithms.data-structures.binary-tree.maximum-fenwick-tree
  "Port of data_structures/binary_tree/maximum_fenwick_tree.py.
  Fenwick tree for range-maximum queries with point updates.
  A tree is {:size n :arr [...] :tree [...]}.")

(defn make-max-fenwick
  "Empty max-Fenwick tree with SIZE slots."
  [size]
  {:size size :arr (vec (repeat size 0)) :tree (vec (repeat size 0))})

(defn get-next
  "Next index after INDEX."
  [index]
  (bit-or index (inc index)))

(defn get-prev
  "Previous index before INDEX."
  [index]
  (dec (bit-and index (inc index))))

(defn max-update
  "Set INDEX to VALUE; returns the new tree."
  [{:keys [size arr tree] :as ft} index value]
  (let [old-value (nth arr index)]
    (if (= value old-value)
      ft
      (loop [arr (assoc arr index value) tree tree i index]
        (if (>= i size)
          (assoc ft :arr arr :tree tree)
          (let [old-max (nth tree i)]
            (cond
              (> value old-max)
              (recur arr (assoc tree i value) (get-next i))
              (not= old-value old-max)
              (assoc ft :arr arr :tree tree)
              :else
              (let [left-border (inc (get-prev i))
                    maximum (if (= left-border i)
                              (nth arr i)
                              (max 0 (nth arr i)))
                    maximum (loop [m maximum child (dec i)]
                              (if (or (< child left-border) (= m old-max))
                                m
                                (recur (max m (nth tree child))
                                       (get-prev child))))
                    tree (assoc tree i maximum)]
                (if (= maximum old-max)
                  (assoc ft :arr arr :tree tree)
                  (recur arr tree (get-next i)))))))))))

(defn max-query
  "Maximum over [LEFT, RIGHT)."
  [{:keys [arr tree]} left right]
  (loop [right (dec right) result 0]
    (if (< right left)
      result
      (let [current-left (get-prev right)]
        (if (<= left current-left)
          (recur current-left (max result (nth tree right)))
          (recur (dec right) (max result (nth arr right))))))))
