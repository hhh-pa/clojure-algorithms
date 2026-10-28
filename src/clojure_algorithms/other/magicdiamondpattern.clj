(ns clojure-algorithms.other.magicdiamondpattern
  "Diamond pattern generation with '*' characters.

  Port of TheAlgorithms/Python `other/magicdiamondpattern.py`.
  Functions return strings instead of printing them.")

(defn floyd
  "Upper half (pyramid) of the diamond for size `n`."
  [n]
  (apply str
         (for [i (range n)]
           (str (apply str (repeat (- n i 1) " "))
                (apply str (repeat (inc i) "* "))
                "\n"))))

(defn reverse-floyd
  "Lower half (inverted pyramid) of the diamond for size `n`."
  [n]
  (apply str
         (for [i (range n 0 -1)]
           (str (apply str (repeat i "* "))
                "\n"
                (apply str (repeat (- n i -1) " "))))))

(defn pretty-print
  "Complete diamond pattern for size `n`."
  [n]
  (if (<= n 0)
    "       ...       ....        nothing printing :("
    (str (floyd n) (reverse-floyd n))))
