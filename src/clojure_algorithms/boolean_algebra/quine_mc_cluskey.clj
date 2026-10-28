(ns clojure-algorithms.boolean-algebra.quine-mc-cluskey
  "Quine-McCluskey prime-implicant helpers.")

(defn compare-string
  "Merges two bit strings differing in one position, else false."
  [s1 s2]
  (let [pairs (map vector s1 s2)
        diffs (count (filter (fn [[a b]] (not= a b)) pairs))]
    (if (> diffs 1)
      false
      (apply str (map (fn [[a b]] (if (not= a b) \_ a)) pairs)))))

(defn check
  "Collects prime implicants from binary minterm strings."
  [binary]
  (loop [binary (vec binary) pi []]
    (let [n (count binary)
          failed? (fn [i j] (false? (compare-string (nth binary i) (nth binary j))))
          failed-pairs (vec (for [i (range n) j (range (inc i) n)
                                  :when (failed? i j)]
                              [i j]))
          starred (into #{} (mapcat identity failed-pairs))
          pi' (vec (concat pi (keep-indexed
                               (fn [i v] (when-not (starred i) v))
                               binary)))]
      (if (empty? failed-pairs)
        pi'
        (recur ["X"] pi')))))

(defn decimal-to-binary
  "Converts integer minterms to n-character bit strings."
  [no-of-variable minterms]
  (mapv (fn [m]
          (loop [m (long m) s ""]
            (if (= (count s) no-of-variable)
              s
              (recur (quot m 2) (str (mod m 2) s)))))
        minterms))

(defn is-for-table
  "True when s1 and s2 differ in exactly count positions."
  [s1 s2 cnt]
  (= cnt (count (filter false? (map = s1 s2)))))

(defn selection
  "Selects essential prime implicants covering the chart."
  [chart prime-implicants]
  (let [chart (atom (mapv vec chart))
        temp (atom [])
        nrows (count @chart)
        ncols (count (first @chart))
        select (boolean-array nrows false)]
    (doseq [i (range ncols)]
      (let [ones (filterv #(= 1 (get-in @chart [% i])) (range nrows))]
        (when (= 1 (count ones))
          (aset select (apply max ones) true))))
    (doseq [i (range nrows)]
      (when (aget select i)
        (doseq [j (range ncols)]
          (when (= 1 (get-in @chart [i j]))
            (doseq [r (range nrows)]
              (swap! chart assoc-in [r j] 0))))
        (swap! temp conj (nth prime-implicants i))))
    (loop []
      (let [counts (mapv #(count (filter #{1} %)) @chart)
            max-n (apply max counts)
            rem (.indexOf ^clojure.lang.IPersistentVector counts max-n)]
        (when-not (zero? max-n)
          (swap! temp conj (nth prime-implicants rem))
          (doseq [j (range ncols)]
            (when (= 1 (get-in @chart [rem j]))
              (doseq [r (range (count @chart))]
                (swap! chart assoc-in [r j] 0))))
          (recur))))
    @temp))

(defn prime-implicant-chart
  "Builds the coverage chart of prime implicants over minterms."
  [prime-implicants binary]
  (mapv (fn [pi]
          (let [cnt (count (filter #{\_} pi))]
            (mapv (fn [b] (if (is-for-table pi b cnt) 1 0)) binary)))
        prime-implicants))
