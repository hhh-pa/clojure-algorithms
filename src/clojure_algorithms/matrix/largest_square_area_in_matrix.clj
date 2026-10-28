(ns clojure-algorithms.matrix.largest-square-area-in-matrix
  "Finds the largest square sub-matrix of 1s in a binary matrix.
  Each function returns the side length of that square.")

(defn- cell-set?
  "Mimics Python truthiness of a grid cell: numbers count when non-zero."
  [cell]
  (if (number? cell) (not (zero? cell)) (boolean cell)))

(defn largest-square-area-in-matrix-top-down-approch
  "Naive top-down recursion (exponential time)."
  [rows cols mat]
  (letfn [(solve [r c]
            (if (or (>= r rows) (>= c cols))
              [0 0]
              (let [[right b1] (solve r (inc c))
                    [diagonal b2] (solve (inc r) (inc c))
                    [down b3] (solve (inc r) c)
                    best (max b1 b2 b3)]
                (if (cell-set? (get-in mat [r c]))
                  (let [s (inc (min right diagonal down))]
                    [s (max best s)])
                  [0 best]))))]
    (second (solve 0 0))))

(defn largest-square-area-in-matrix-top-down-approch-with-dp
  "Top-down recursion memoized over a dp table (O(rows*cols) time)."
  [rows cols mat]
  (letfn [(solve [r c state]
            (if (or (>= r rows) (>= c cols))
              [0 state]
              (let [{:keys [dp]} state]
                (if (contains? dp [r c])
                  [(dp [r c]) state]
                  (let [[right s1] (solve r (inc c) state)
                        [diagonal s2] (solve (inc r) (inc c) s1)
                        [down s3] (solve (inc r) c s2)]
                    (if (cell-set? (get-in mat [r c]))
                      (let [s (inc (min right diagonal down))]
                        [s (-> s3
                               (assoc-in [:dp [r c]] s)
                               (update :best max s))])
                      [0 s3]))))))]
    (:best (second (solve 0 0 {:dp {} :best 0})))))

(defn largest-square-area-in-matrix-bottom-up
  "Bottom-up dynamic programming over a full dp table."
  [rows cols mat]
  (second
   (reduce (fn [[dp best] r]
             (reduce (fn [[dp best] c]
                       (if (= 1 (get-in mat [r c]))
                         (let [s (inc (min (get-in dp [r (inc c)])
                                           (get-in dp [(inc r) (inc c)])
                                           (get-in dp [(inc r) c])))]
                           [(assoc-in dp [r c] s) (max best s)])
                         [(assoc-in dp [r c] 0) best]))
                     [dp best]
                     (range (dec cols) -1 -1)))
           [(vec (repeat (inc rows) (vec (repeat (inc cols) 0)))) 0]
           (range (dec rows) -1 -1))))

(defn largest-square-area-in-matrix-bottom-up-space-optimization
  "Bottom-up dynamic programming keeping only two rows."
  [rows cols mat]
  (second
   (reduce (fn [[next-row best] r]
             (let [[current best]
                   (reduce (fn [[current best] c]
                             (if (= 1 (get-in mat [r c]))
                               (let [s (inc (min (nth current (inc c))
                                                 (nth next-row (inc c))
                                                 (nth next-row c)))]
                                 [(assoc current c s) (max best s)])
                               [(assoc current c 0) best]))
                           [(vec (repeat (inc cols) 0)) best]
                           (range (dec cols) -1 -1))]
               [current best]))
           [(vec (repeat (inc cols) 0)) 0]
           (range (dec rows) -1 -1))))
