(ns clojure-algorithms.sorts.flash-sort
  "Flash sort: distribution sort with O(n) behaviour on uniformly
  distributed numeric data. Ported from TheAlgorithms/Python.")

(declare flash-permute!)

(defn flash-sort
  "Sorts numeric `coll` with flash sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (<= n 1)
      v
      (let [lo (apply min v)
            hi (apply max v)]
        (if (= lo hi)
          v
          (let [arr (object-array v)
                m (max 1 (int (* 0.45 n)))
                class-sizes (long-array m)
                c1 (/ (double (dec m)) (- (double hi) (double lo)))
                class-of (fn [x]
                           (min (int (* c1 (- (double x) (double lo))))
                                (dec m)))]
            (doseq [x v]
              (let [c (class-of x)]
                (aset class-sizes c (inc (aget class-sizes c)))))
            (loop [i 1]
              (when (< i m)
                (aset class-sizes i (+ (aget class-sizes i)
                                       (aget class-sizes (dec i))))
                (recur (inc i))))
            (flash-permute! arr class-sizes class-of n m)
            ;; insertion sort straightens each class
            (loop [i 1]
              (when (< i n)
                (let [k (aget arr i)]
                  (loop [j (dec i)]
                    (if (and (>= j 0)
                             (pos? (compare (aget arr j) k)))
                      (do (aset arr (inc j) (aget arr j))
                          (recur (dec j)))
                      (aset arr (inc j) k))))
                (recur (inc i))))
            (vec arr)))))))
(defn- flash-permute!
  "In-place cycle-leader permutation phase of flash sort."
  [arr class-sizes class-of n m]
  (loop [moves 0 j 0 k (dec m)]
    (when (< moves (dec n))
      (let [[j2 k2] (loop [jj j kk k]
                      (if (> jj (dec (aget class-sizes kk)))
                        (let [jj2 (inc jj)]
                          (recur jj2 (class-of (aget arr jj2))))
                        [jj kk]))
            [moves2 k3] (loop [fl (aget arr j2) kk k2 mv moves]
                          (if (= j2 (aget class-sizes kk))
                            [mv kk]
                            (let [kk2 (class-of fl)]
                              (aset class-sizes kk2
                                    (dec (aget class-sizes kk2)))
                              (let [tmp (aget arr (aget class-sizes kk2))]
                                (aset arr (aget class-sizes kk2) fl)
                                (recur tmp kk2 (inc mv))))))]
        (recur moves2 j2 k3)))))
