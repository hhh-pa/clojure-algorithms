(ns clojure-algorithms.strings.manacher
  "Manacher's algorithm: longest palindromic substring in linear time.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn palindromic-string
  "Longest palindromic substring of `s`."
  [s]
  (if (empty? s)
    ""
    (let [t (str/join "|" (map str s))
          n (count t)]
      (loop [j 0 left 0 right 0 lengths (vec (repeat n 1))
             max-len 0 start 0]
        (if (>= j n)
          (str/replace (subs t (- start (quot max-len 2))
                             (+ start (quot max-len 2) 1))
                       "|" "")
          (let [k (if (> j right)
                    1
                    (min (quot (nth lengths (- (+ left right) j)) 2)
                         (inc (- right j))))
                k2 (loop [k k]
                     (if (and (>= (- j k) 0)
                              (< (+ j k) n)
                              (= (nth t (+ j k)) (nth t (- j k))))
                       (recur (inc k))
                       k))
                lengths2 (assoc lengths j (dec (* 2 k2)))
                [left2 right2] (if (> (+ j k2 -1) right)
                                 [(- j k2 -1) (+ j k2 -1)]
                                 [left right])
                [max-len2 start2] (if (> (nth lengths2 j) max-len)
                                    [(nth lengths2 j) j]
                                    [max-len start])]
            (recur (inc j) left2 right2 lengths2 max-len2 start2)))))))
