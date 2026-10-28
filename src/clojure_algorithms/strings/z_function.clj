(ns clojure-algorithms.strings.z-function
  "Z-function (Z algorithm) and Z-based pattern counting.
  See https://cp-algorithms.com/string/z-function.html
  Ported from TheAlgorithms/Python.")

(defn- go-next?
  [i z s]
  (and (< (+ i (nth z i)) (count s))
       (= (nth s (nth z i)) (nth s (+ i (nth z i))))))

(defn z-function
  "Z-array for `s`: z[i] is the longest prefix match starting at i."
  [s]
  (let [n (count s)]
    (loop [i 1 z (vec (repeat n 0)) l 0 r 0]
      (if (>= i n)
        z
        (let [z2 (if (<= i r)
                   (assoc z i (min (- r i -1) (nth z (- i l))))
                   z)
              z3 (loop [zz z2]
                   (if (go-next? i zz s)
                     (recur (update zz i inc))
                     zz))
              [l2 r2] (if (> (+ i (nth z3 i) -1) r)
                        [i (+ i (nth z3 i) -1)]
                        [l r])]
          (recur (inc i) z3 l2 r2))))))

(defn find-pattern
  "Number of occurrences of `pattern` in `s` via the Z-function."
  [pattern s]
  (let [m (count pattern)]
    (count (filter #(>= % m) (drop m (z-function (str pattern s)))))))
