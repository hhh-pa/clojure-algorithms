(ns clojure-algorithms.data-structures.binary-tree.segment-tree
  "Port of data_structures/binary_tree/segment_tree.py.
  Range-max segment tree with range assignment (1-indexed ranges).
  State is {:n :a :st}; updates return [new-state true].")

(defn left-index
  "Left child index of IDX."
  [idx]
  (* idx 2))

(defn right-index
  "Right child index of IDX."
  [idx]
  (inc (* idx 2)))

(defn- build-at [st idx l r a]
  (if (= l r)
    (assoc-in st [:st idx] (nth a l))
    (let [mid (quot (+ l r) 2)
          st (build-at st (left-index idx) l mid a)
          st (build-at st (right-index idx) (inc mid) r a)]
      (assoc-in st [:st idx]
                (max (get-in st [:st (left-index idx)])
                     (get-in st [:st (right-index idx)]))))))

(defn make-seg-tree
  "Segment tree over vector A."
  [a]
  (let [st {:n (count a) :a (vec a) :st (vec (repeat (* 4 (count a)) 0))}]
    (if (zero? (count a)) st (build-at st 1 0 (dec (count a)) (vec a)))))

(defn seg-update
  "Assign VAL over 1-indexed range [A, B]; returns [new-state true]."
  [st a b val]
  (letfn [(upd [st idx l r]
            (cond
              (or (< r (dec a)) (> l (dec b))) st
              (= l r) (assoc-in st [:st idx] val)
              :else (let [mid (quot (+ l r) 2)
                          st (upd st (left-index idx) l mid)
                          st (upd st (right-index idx) (inc mid) r)]
                      (assoc-in st [:st idx]
                                (max (get-in st [:st (left-index idx)])
                                     (get-in st [:st (right-index idx)]))))))]
    [(upd st 1 0 (dec (:n st))) true]))

(defn seg-query
  "Maximum over 1-indexed range [A, B]."
  [st a b]
  (letfn [(qry [idx l r]
            (cond
              (or (< r (dec a)) (> l (dec b))) ##-Inf
              (and (>= l (dec a)) (<= r (dec b))) (get-in st [:st idx])
              :else (let [mid (quot (+ l r) 2)]
                      (max (qry (left-index idx) l mid)
                           (qry (right-index idx) (inc mid) r)))))]
    (qry 1 0 (dec (:n st)))))

(defn show-data
  "Current array values."
  [st]
  (mapv #(seg-query st % %) (range 1 (inc (:n st)))))
