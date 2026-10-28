(ns clojure-algorithms.ciphers.rot13
  "ROT13 / general Caesar rotation.")

(defn dencrypt
  "Rotates ASCII letters of s by n places (default 13)."
  ([s] (dencrypt s 13))
  ([s n]
   (apply str (map (fn [c]
                     (cond (<= (int \A) (int c) (int \Z))
                           (char (+ (int \A) (mod (+ (- (int c) (int \A)) n) 26)))
                           (<= (int \a) (int c) (int \z))
                           (char (+ (int \a) (mod (+ (- (int c) (int \a)) n) 26)))
                           :else c))
                   s))))
