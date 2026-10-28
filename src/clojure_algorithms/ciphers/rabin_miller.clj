(ns clojure-algorithms.ciphers.rabin-miller
  "Rabin-Miller primality testing and large prime generation.")

(def low-primes
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
   101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191
   193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
   293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401
   409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509
   521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631
   641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751
   757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877
   881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997])

(defn- mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn rabin-miller
  "Probabilistic Miller-Rabin test with 5 random witnesses."
  [num]
  (let [num (bigint num)
        [s t] (loop [s (dec num) t 0]
                (if (even? s) (recur (quot s 2) (inc t)) [s t]))]
    (every? (fn [_]
              (let [span (- num 3)
                    a (if (<= span 1)
                        (bigint 2)
                        (+ 2 (bigint (rand-int (int (min span Integer/MAX_VALUE))))))
                    v (mod-pow a s num)]
                (if (.equals ^BigInteger v BigInteger/ONE)
                  true
                  (loop [i 0 v v]
                    (cond (.equals ^BigInteger v
                                   (.subtract (biginteger num) BigInteger/ONE)) true
                          (= i (dec t)) false
                          :else (recur (inc i)
                                       (.mod (.multiply ^BigInteger v
                                                        ^BigInteger v)
                                             (biginteger num))))))))
            (range 5))))

(defn is-prime-low-num
  "Trial division by small primes, then Rabin-Miller."
  [num]
  (let [num (bigint num)]
    (cond
      (< num 2) false
      (contains? (set (map bigint low-primes)) num) true
      (some #(zero? (mod num %)) low-primes) false
      :else (boolean (rabin-miller num)))))

(defn generate-large-prime
  "Generates a random prime with keysize bits."
  ([] (generate-large-prime 1024))
  ([keysize]
   (loop []
     (let [num (+ (.shiftLeft BigInteger/ONE (dec keysize))
                  (BigInteger. (dec keysize) (java.util.Random.)))]
       (if (is-prime-low-num num) num (recur))))))
