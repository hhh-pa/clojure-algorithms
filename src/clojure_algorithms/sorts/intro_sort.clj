(ns clojure-algorithms.sorts.intro-sort
  "Introsort: hybrid of quick sort, heap sort and insertion sort.
  Ported from TheAlgorithms/Python."
  (:refer-clojure :exclude [sort]))

(defn insertion-sort-range
  "Insertion-sorts the half-open range [start, end) of vector `v`."
  [v start end]
  (loop [a v i start]
    (if (>= i end)
      a
      (let [x (nth a i)]
        (recur (loop [b a j i]
                 (if (and (> j start)
                          (neg? (compare x (nth b (dec j)))))
                   (recur (assoc b j (nth b (dec j))) (dec j))
                   (assoc b j x)))
               (inc i))))))

(defn heapify
  "Restores the max-heap property at heap index `idx` for a heap of
  `heap-size` elements starting at `start` in vector `v`."
  [v idx heap-size start]
  (let [l (inc (* 2 idx))
        r (+ 2 (* 2 idx))
        m (if (and (< l heap-size)
                   (neg? (compare (nth v (+ start idx))
                                  (nth v (+ start l)))))
            l idx)
        m2 (if (and (< r heap-size)
                    (neg? (compare (nth v (+ start m))
                                   (nth v (+ start r)))))
             r m)]
    (if (= m2 idx)
      v
      (recur (assoc v (+ start idx) (nth v (+ start m2))
                    (+ start m2) (nth v (+ start idx)))
             m2 heap-size start))))

(defn heap-sort-range
  "Heap-sorts the half-open range [start, end) of vector `v`."
  [v start end]
  (let [n (- end start)
        heap (reduce (fn [a i] (heapify a i n start))
                     v (range (dec (quot n 2)) -1 -1))]
    (loop [a heap i (dec n)]
      (if (<= i 0)
        a
        (recur (heapify (assoc a (+ start i) (nth a start)
                               start (nth a (+ start i)))
                        0 i start)
               (dec i))))))

(defn median-of-3
  "Median of the elements at the three given indices of `v`."
  [v first-idx middle-idx last-idx]
  (let [a (nth v first-idx) b (nth v middle-idx) c (nth v last-idx)]
    (cond
      (not= (neg? (compare b a)) (neg? (compare c a))) a
      (not= (neg? (compare a b)) (neg? (compare c b))) b
      :else c)))

(defn partition-by-pivot
  "Hoare-style partition of [low, high) around `pivot`.
  Returns [v split-index]."
  [v low high pivot]
  (loop [a v i low j high]
    (let [i2 (loop [k i] (if (neg? (compare (nth a k) pivot)) (recur (inc k)) k))
          j2 (loop [k (dec j)] (if (neg? (compare pivot (nth a k))) (recur (dec k)) k))]
      (if (>= i2 j2)
        [a i2]
        (recur (assoc a i2 (nth a j2) j2 (nth a i2)) (inc i2) j2)))))

(defn intro-sort-range
  "Introsorts the half-open range [start, end) of vector `v`."
  [v start end size-threshold max-depth]
  (loop [a v s start e end d max-depth]
    (if (<= (- e s) size-threshold)
      (insertion-sort-range a s e)
      (if (zero? d)
        (heap-sort-range a s e)
        (let [pivot (median-of-3 a s (+ s (quot (- e s) 2) 1) (dec e))
              [a2 p] (partition-by-pivot a s e pivot)
              a3 (intro-sort-range a2 p e size-threshold (dec d))]
          (recur a3 s p (dec d)))))))

(defn intro-sort
  "Sorts `coll` with introsort (this port's name for Python's `sort`).
  Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      v
      (let [max-depth (* 2 (int (Math/ceil (/ (Math/log (count v))
                                              (Math/log 2)))))]
        (intro-sort-range v 0 (count v) 16 max-depth)))))
