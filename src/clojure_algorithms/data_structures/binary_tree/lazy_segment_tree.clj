(ns clojure-algorithms.data-structures.binary-tree.lazy-segment-tree
  "Port of data_structures/binary_tree/lazy_segment_tree.py.
  Range-max segment tree with lazy range assignment (1-indexed ranges).
  State is {:size :tree :lazy :flag}; updates return [new-state true].")

(defn make-seg-tree
  "Empty lazy segment tree for SIZE elements."
  [size]
  {:size size
   :tree (vec (repeat (* 4 size) 0))
   :lazy (vec (repeat (* 4 size) 0))
   :flag (vec (repeat (* 4 size) false))})

(defn left-index
  "Left child index of IDX."
  [idx]
  (* idx 2))

(defn right-index
  "Right child index of IDX."
  [idx]
  (inc (* idx 2)))

(defn seg-build
  "Build over 1-indexed range [L, R] from vector A; returns new state."
  [st idx l r a]
  (if (= l r)
    (assoc-in st [:tree idx] (nth a (dec l)))
    (let [mid (quot (+ l r) 2)
          st (seg-build st (left-index idx) l mid a)
          st (seg-build st (right-index idx) (inc mid) r a)]
      (assoc-in st [:tree idx]
                (max (get-in st [:tree (left-index idx)])
                     (get-in st [:tree (right-index idx)]))))))

(defn- push-down [st idx l r]
  (if-not (get-in st [:flag idx])
    st
    (let [v (get-in st [:lazy idx])
          st (-> st
                 (assoc-in [:tree idx] v)
                 (assoc-in [:flag idx] false))]
      (if (= l r)
        st
        (-> st
            (assoc-in [:lazy (left-index idx)] v)
            (assoc-in [:lazy (right-index idx)] v)
            (assoc-in [:flag (left-index idx)] true)
            (assoc-in [:flag (right-index idx)] true))))))

(defn seg-update
  "Assign VAL over 1-indexed range [A, B]; returns [new-state true]."
  [st idx l r a b val]
  (let [st (push-down st idx l r)]
    (cond
      (or (< r a) (> l b)) [st true]
      (and (>= l a) (<= r b))
      [(cond-> (assoc-in st [:tree idx] val)
         (not= l r) (assoc-in [:lazy (left-index idx)] val)
         (not= l r) (assoc-in [:lazy (right-index idx)] val)
         (not= l r) (assoc-in [:flag (left-index idx)] true)
         (not= l r) (assoc-in [:flag (right-index idx)] true))
       true]
      :else (let [mid (quot (+ l r) 2)
                  [st _] (seg-update st (left-index idx) l mid a b val)
                  [st _] (seg-update st (right-index idx) (inc mid) r a b val)]
              [(assoc-in st [:tree idx]
                         (max (get-in st [:tree (left-index idx)])
                              (get-in st [:tree (right-index idx)])))
               true]))))

(defn seg-query
  "Maximum over 1-indexed range [A, B]."
  [st idx l r a b]
  (let [st (push-down st idx l r)]
    (cond
      (or (< r a) (> l b)) ##-Inf
      (and (>= l a) (<= r b)) (get-in st [:tree idx])
      :else (let [mid (quot (+ l r) 2)]
              (max (seg-query st (left-index idx) l mid a b)
                   (seg-query st (right-index idx) (inc mid) r a b))))))

(defn seg-values
  "Current array values."
  [st]
  (mapv #(seg-query st 1 1 (:size st) % %) (range 1 (inc (:size st)))))
