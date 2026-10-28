(ns clojure-algorithms.project-euler.problem-345.sol1
  "Project Euler Problem 345: matrix sum.

  The Matrix Sum of a matrix is the maximum possible sum of matrix elements
  such that none of the selected elements share the same row or column. Uses
  a brute force solution with caching of intermediate steps."
  (:require [clojure.string :as str]))

(def matrix-1
  "Example 5x5 matrix with matrix sum 3315."
  ["7 53 183 439 863"
   "497 383 563 79 973"
   "287 63 343 169 583"
   "627 343 773 959 943"
   "767 473 103 699 303"])

(def matrix-2
  "Target 15x15 matrix."
  ["7 53 183 439 863 497 383 563 79 973 287 63 343 169 583"
   "627 343 773 959 943 767 473 103 699 303 957 703 583 639 913"
   "447 283 463 29 23 487 463 993 119 883 327 493 423 159 743"
   "217 623 3 399 853 407 103 983 89 463 290 516 212 462 350"
   "960 376 682 962 300 780 486 502 912 800 250 346 172 812 350"
   "870 456 192 162 593 473 915 45 989 873 823 965 425 329 803"
   "973 965 905 919 133 673 665 235 509 613 673 815 165 992 326"
   "322 148 972 962 286 255 941 541 265 323 925 281 601 95 973"
   "445 721 11 525 473 65 511 164 138 672 18 428 154 448 848"
   "414 456 310 312 798 104 566 520 302 248 694 976 430 392 198"
   "184 829 373 181 631 101 969 613 840 740 778 458 284 760 390"
   "821 461 843 513 17 901 711 993 293 157 274 94 192 156 574"
   "34 124 4 878 450 476 712 914 838 669 875 299 823 329 699"
   "815 559 813 459 522 788 168 586 966 232 308 833 251 631 107"
   "813 883 451 509 615 77 281 613 459 205 380 274 302 35 805"])

(defn solve
  "Finds the max sum for array arr starting with row index row, with columns
  included in cols. The cache atom stores intermediate results."
  [arr row cols cache]
  (let [cache-id [row cols]]
    (if-let [cached (get @cache cache-id)]
      cached
      (if (= row (count arr))
        0
        (let [max-sum (reduce (fn [best col]
                                (max best
                                     (+ (get-in arr [row col])
                                        (solve arr
                                               (inc row)
                                               (disj cols col)
                                               cache))))
                              0
                              cols)]
          (swap! cache assoc cache-id max-sum)
          max-sum)))))

(defn solution
  "Takes a list of strings matrix-str to parse the matrix and calculates
  the max sum."
  ([] (solution matrix-2))
  ([matrix-str]
   (let [n (count matrix-str)
         arr (mapv (fn [row-str]
                     (mapv #(Integer/parseInt %)
                           (str/split row-str #"\s+")))
                   matrix-str)]
     (solve arr 0 (set (range n)) (atom {})))))
