(ns clojure-algorithms.strings.booths-algorithm
  "Booth's algorithm: lexicographically minimal string rotation in O(n).
  Ported from TheAlgorithms/Python.")

(defn find-minimal-rotation
  "Returns the lexicographically minimal rotation of non-empty string `s`.
  Throws IllegalArgumentException for empty input."
  [s]
  (when (or (not (string? s)) (empty? s))
    (throw (IllegalArgumentException. "Input must be a non-empty string")))
  (let [n (count s)
        doubled (str s s)
        f (long-array (* 2 n) -1)]
    (loop [j 1 k 0]
      (if (>= j (* 2 n))
        (subs doubled k (+ k n))
        (let [sj (nth doubled j)
              i (aget f (- j k 1))
              [i2 k2] (loop [ii i kk k]
                        (if (and (not= ii -1)
                                 (not= sj (nth doubled (+ kk ii 1))))
                          (recur (aget f ii)
                                 (if (< (int sj)
                                        (int (nth doubled (+ kk ii 1))))
                                   (- j ii 1) kk))
                          [ii kk]))
              [k3] (if (and (= i2 -1) (not= sj (nth doubled k2)))
                     [(if (< (int sj) (int (nth doubled k2))) j k2)]
                     [k2])]
          (aset f (- j k3) (if (and (= i2 -1) (not= sj (nth doubled k2)))
                             -1 (inc i2)))
          (recur (inc j) k3))))))
