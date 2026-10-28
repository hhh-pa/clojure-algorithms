(ns clojure-algorithms.sorts.smoothsort
  "Smoothsort: Dijkstra's adaptive sort over a forest of Leonardo heaps.
  Ported from TheAlgorithms/Python.")

(def leonardo
  "Leonardo numbers L(0)=1, L(1)=1, L(k)=L(k-1)+L(k-2)+1."
  (vec (map first
            (take-while (fn [[a _]] (< a (bit-shift-left 1 31)))
                        (iterate (fn [[a b]] [b (+ a b 1)]) [1 1])))))

(def ^:private leo leonardo)

(defn sift
  "Restores the max-heap property of the Leonardo tree of `order`
  rooted at `root` in vector `v`. Returns the updated vector."
  [v root order]
  (loop [a v r root o order]
    (if (<= o 1)
      a
      (let [right (dec r)
            left (- r 1 (nth leo (- o 2)))
            l (nth a left) rr (nth a right) x (nth a r)]
        (cond
          (and (not (neg? (compare l rr))) (neg? (compare x l)))
          (recur (assoc a r l left x) left (dec o))
          (and (neg? (compare l rr)) (neg? (compare x rr)))
          (recur (assoc a r rr right x) right (- o 2))
          :else a)))))

(defn trinkle
  "Restores root ordering and heap ordering for the tree rooted at `pos`.
  `heap-sizes` holds Leonardo orders left-to-right; `idx` is the position
  of this tree in it. Returns the updated vector."
  [v pos heap-sizes idx]
  (let [[a p i]
        (loop [a v p pos i idx]
          (if (zero? i)
            [a p i]
            (let [prev (- p (nth leo (nth heap-sizes i)))]
              (if (not (neg? (compare (nth a p) (nth a prev))))
                [a p i]
                (if (and (> (nth heap-sizes i) 1)
                         (let [right (dec p)
                               left (- p 1 (nth leo (- (nth heap-sizes i) 2)))]
                           (or (not (neg? (compare (nth a right)
                                                   (nth a prev))))
                               (not (neg? (compare (nth a left)
                                                   (nth a prev)))))))
                  [a p i]
                  (recur (assoc a p (nth a prev) prev (nth a p))
                         prev (dec i)))))))]
    (sift a p (nth heap-sizes i))))

(defn smoothsort
  "Sorts `coll` with smoothsort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (< n 2)
      v
      (let [[built heaps]
            (loop [a v i 0 hs []]
              (if (>= i n)
                [a hs]
                (let [hs2 (cond
                            (and (>= (count hs) 2)
                                 (= (nth hs (- (count hs) 2))
                                    (inc (peek hs))))
                            (let [dropped (pop hs)]
                              (assoc dropped (dec (count dropped))
                                     (inc (peek dropped))))

                            (and (seq hs) (= 1 (peek hs)))
                            (conj hs 0)

                            :else (conj hs 1))]
                  (recur (trinkle a i hs2 (dec (count hs2)))
                         (inc i) hs2))))]
        (loop [a built i (dec n) hs heaps]
          (if (neg? i)
            a
            (let [order (peek hs)
                  hs2 (pop hs)]
              (if (> order 1)
                (let [right-order (- order 2)
                      left-order (dec order)
                      right-pos (dec i)
                      left-pos (- i 1 (nth leo right-order))
                      hs3 (conj hs2 left-order)
                      a2 (trinkle a left-pos hs3 (dec (count hs3)))
                      hs4 (conj hs3 right-order)
                      a3 (trinkle a2 right-pos hs4 (dec (count hs4)))]
                  (recur a3 (dec i) hs4))
                (recur a (dec i) hs2)))))))))
