(ns clojure-algorithms.data-structures.arrays.find-triplets-with-0-sum
  "Port of data_structures/arrays/find_triplets_with_0_sum.py.
  Three ways to find unique triplets summing to zero.")

(defn- combinations-3 [coll]
  (let [v (vec coll) n (count v)]
    (for [i (range n) j (range (inc i) n) k (range (inc j) n)]
      [(nth v i) (nth v j) (nth v k)])))

(defn find-triplets-with-0-sum
  "Unique sorted triplets summing to zero (brute force over combinations)."
  [nums]
  (mapv vec (sort (set (filter #(zero? (reduce + %))
                               (combinations-3 (sort nums)))))))

(defn find-triplets-with-0-sum-hashing
  "Unique triplets summing to zero via the hashing approach."
  [arr]
  (let [v (vec arr)]
    (loop [index 0 output []]
      (if (>= index (- (count v) 2))
        output
        (let [item (nth v index)
              current-sum (- item)
              [_ new-output]
              (loop [js (range (inc index) (count v)) seen #{} out output]
                (if (empty? js)
                  [seen out]
                  (let [other (nth v (first js))
                        required (- current-sum other)]
                    (recur (rest js)
                           (conj seen other)
                           (if (contains? seen required)
                             (let [combo (vec (sort [item other required]))]
                               (if (some #(= combo %) out) out (conj out combo)))
                             out)))))]
          (recur (inc index) new-output))))))

(defn find-triplets-with-0-sum-two-pointers
  "Unique triplets summing to zero via the two-pointer technique."
  [nums]
  (let [v (vec (sort nums)) n (count v)]
    (loop [i 0 result []]
      (if (>= i (- n 2))
        result
        (if (and (pos? i) (= (nth v i) (nth v (dec i))))
          (recur (inc i) result)
          (let [res (loop [left (inc i) right (dec n) acc result]
                      (if (>= left right)
                        acc
                        (let [total (+ (nth v i) (nth v left) (nth v right))]
                          (cond
                            (zero? total)
                            (let [acc (conj acc [(nth v i) (nth v left)
                                                 (nth v right)])
                                  left (loop [l (inc left)]
                                         (if (and (< l right)
                                                  (= (nth v l) (nth v (dec l))))
                                           (recur (inc l)) l))
                                  right (loop [r (dec right)]
                                          (if (and (< left r)
                                                   (= (nth v r) (nth v (inc r))))
                                            (recur (dec r)) r))]
                              (recur left right acc))
                            (neg? total) (recur (inc left) right acc)
                            :else (recur left (dec right) acc)))))]
            (recur (inc i) res)))))))
