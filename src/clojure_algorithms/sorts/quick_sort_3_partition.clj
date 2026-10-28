(ns clojure-algorithms.sorts.quick-sort-3-partition
  "Quicksort variants: 3-way partition, Lomuto, Hoare and three-way radix.
  Ported from TheAlgorithms/Python.")

(defn quick-sort-3partition
  "In-place-style 3-way quicksort of [left, right] (inclusive) of `v`.
  Returns the sorted vector."
  ([coll] (quick-sort-3partition (vec coll) 0 (dec (count coll))))
  ([v left right]
   (if (<= right left)
     v
     (let [pivot (nth v left)
           [a2 a-pt b-pt]
           (loop [a v i left a-pt left b-pt right]
             (if (> i b-pt)
               [a a-pt b-pt]
               (let [c (compare (nth a i) pivot)]
                 (cond
                   (neg? c)
                   (recur (assoc a a-pt (nth a i) i (nth a a-pt))
                          (inc i) (inc a-pt) b-pt)
                   (pos? c)
                   (recur (assoc a b-pt (nth a i) i (nth a b-pt))
                          i a-pt (dec b-pt))
                   :else (recur a (inc i) a-pt b-pt)))))]
       (-> a2
           (quick-sort-3partition left (dec a-pt))
           (quick-sort-3partition (inc b-pt) right))))))

(defn lomuto-partition
  "Lomuto partition of [left, right] (inclusive) around the last element.
  Returns [v pivot-index]."
  [v left right]
  (let [pivot (nth v right)
        [a2 store]
        (reduce (fn [[a s] i]
                  (if (neg? (compare (nth a i) pivot))
                    [(assoc a s (nth a i) i (nth a s)) (inc s)]
                    [a s]))
                [v left] (range left right))]
    [(assoc a2 right (nth a2 store) store (nth a2 right)) store]))

(defn quick-sort-lomuto-partition
  "Quicksort with Lomuto partition over [left, right] (inclusive).
  Returns the sorted vector."
  ([coll] (quick-sort-lomuto-partition (vec coll) 0 (dec (count coll))))
  ([v left right]
   (if (< left right)
     (let [[a2 p] (lomuto-partition v left right)]
       (-> a2
           (quick-sort-lomuto-partition left (dec p))
           (quick-sort-lomuto-partition (inc p) right)))
     v)))

(defn hoare-partition-by-value
  "Partitions [start, end] (inclusive) around `pivot-value`.
  Returns [v right-start-index]."
  ([v pivot-value] (hoare-partition-by-value v pivot-value 0 (dec (count v))))
  ([v pivot-value start end]
   (loop [a v left start right end]
     (let [l (loop [k left]
               (cond (> k end) (inc end)
                     (neg? (compare (nth a k) pivot-value)) (recur (inc k))
                     :else k))]
       (if (> l end)
         [a (inc end)]
         (let [r (loop [k right]
                   (cond (< k start) (dec start)
                         (not (neg? (compare (nth a k) pivot-value)))
                         (recur (dec k))
                         :else k))]
           (if (< r start)
             [a start]
             (if (> l r)
               [a (inc r)]
               (recur (assoc a l (nth a r) r (nth a l))
                      (inc l) (dec r))))))))))

(defn hoare-partition-by-pivot
  "Partitions [start, end] (inclusive) around the element at `pivot-idx`.
  Returns [v new-pivot-index]."
  ([v pivot-idx] (hoare-partition-by-pivot v pivot-idx 0 (dec (count v))))
  ([v pivot-idx start end]
   (let [pv (nth v pivot-idx)
         a (assoc v pivot-idx (nth v end) end pv)
         [a2 ge] (hoare-partition-by-value a pv start (dec end))]
     [(assoc a2 end (nth a2 ge) ge (nth a2 end)) ge])))

(defn quicksort-hoare
  "Quicksort with the Hoare partition scheme. Returns a sorted vector."
  ([coll] (quicksort-hoare (vec coll) 0 (dec (count coll))))
  ([v start end]
   (if (<= (inc (- end start)) 1)
     v
     (let [pivot-idx (+ start (rand-int (max 1 (- end start))))
           [a2 p] (hoare-partition-by-pivot v pivot-idx start end)]
       (-> a2
           (quicksort-hoare start (dec p))
           (quicksort-hoare (inc p) end))))))

(defn three-way-radix-quicksort
  "Three-way radix quicksort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [p (first v)]
        (into (into (three-way-radix-quicksort
                     (filterv #(neg? (compare % p)) v))
                    (filterv #(zero? (compare % p)) v))
              (three-way-radix-quicksort
               (filterv #(pos? (compare % p)) v)))))))
