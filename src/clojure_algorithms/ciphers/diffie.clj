(ns clojure-algorithms.ciphers.diffie
  "Primitive-root search for Diffie-Hellman demos.")

(defn- mod-pow [b e m]
  (long (.modPow (biginteger b) (biginteger e) (biginteger m))))

(defn find-primitive
  "Returns a primitive root mod modulus, or nil when none exists."
  [modulus]
  (first (filter (fn [r]
                   (loop [x 0 seen #{}]
                     (cond (>= x (dec modulus)) true
                           (contains? seen (mod-pow r x modulus)) false
                           :else (recur (inc x)
                                        (conj seen (mod-pow r x modulus))))))
                 (range 1 modulus))))
