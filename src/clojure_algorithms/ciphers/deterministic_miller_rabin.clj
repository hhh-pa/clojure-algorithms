(ns clojure-algorithms.ciphers.deterministic-miller-rabin
  "Deterministic Miller-Rabin primality test.")

(def ^:private bounds
  [2047 1373653 25326001 3215031751 2152302898747 3474749660383
   341550071728321 1 3825123056546413051 1 1 318665857834031151167461
   3317044064679887385961981])

(def ^:private primes [2 3 5 7 11 13 17 19 23 29 31 37 41])

(defn- mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn miller-rabin
  "Deterministic Miller-Rabin; true means prime (or probably prime)."
  ([n] (miller-rabin n false))
  ([n allow-probable]
   (cond
     (= n 2) true
     (or (even? n) (< n 2)) false
     (and (> n 5) (not (#{1 3 7 9} (mod n 10)))) false
     (and (> n 3317044064679887385961981) (not allow-probable))
     (throw (IllegalArgumentException.
             (str "Warning: upper bound of deterministic test is exceeded. "
                  "Pass allow-probable=true to allow probabilistic test.")))
     :else
     (let [plist (or (some (fn [idx] (when (< n (nth bounds idx))
                                       (subvec primes 0 (inc idx))))
                           (range (count bounds)))
                     primes)
           [d s] (loop [d (bigint (dec n)) s 0]
                   (if (even? d) (recur (quot d 2) (inc s)) [d s]))
           n-big (biginteger n)]
       (every? (fn [p]
                 (loop [r 0]
                   (if (>= r s)
                     false
                     (let [m (mod-pow p (.multiply (biginteger d) (.shiftLeft BigInteger/ONE r)) n)]
                       (cond (and (zero? r) (.equals ^BigInteger m BigInteger/ONE)) true
                             (.equals (.add ^BigInteger m BigInteger/ONE) n-big) true
                             :else (recur (inc r)))))))
               plist)))))
