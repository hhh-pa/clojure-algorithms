(ns clojure-algorithms.data-structures.binary-tree.non-recursive-segment-tree
  "Port of data_structures/binary_tree/non_recursive_segment_tree.py.
  Iterative segment tree over [0, N) with a combine function F.
  State is {:n :st :f}; queries use inclusive bounds.")

(defn make-seg-tree
  "Segment tree over ARR with combine function F."
  [arr f]
  (let [n (count arr)
        st (vec (concat (repeat n nil) arr))
        st (loop [st st p (dec n)]
             (if (< p 1) st
                 (recur (assoc st p (f (nth st (* p 2))
                                       (nth st (inc (* p 2)))))
                        (dec p))))]
    {:n n :st st :f f}))

(defn seg-update
  "Set position P to V; returns the new tree."
  [{:keys [n st f] :as s} p v]
  (let [st (assoc st (+ p n) v)]
    (assoc s :st
           (loop [st st p (+ p n)]
             (if (<= p 1) st
                 (let [p (quot p 2)]
                   (recur (assoc st p (f (nth st (* p 2))
                                         (nth st (inc (* p 2)))))
                          p)))))))

(defn seg-query
  "Combine over inclusive [LEFT, RIGHT]."
  [{:keys [n st f]} left right]
  (loop [l (+ left n) r (+ right n) res nil]
    (if (> l r)
      res
      (let [res (if (odd? l)
                  (if (nil? res) (nth st l) (f res (nth st l))) res)
            res (if (even? r)
                  (if (nil? res) (nth st r) (f res (nth st r))) res)]
        (recur (quot (inc l) 2) (quot (dec r) 2) res)))))
