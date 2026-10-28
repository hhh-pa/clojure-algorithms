(ns clojure-algorithms.ciphers.rail-fence-cipher
  "Rail fence (zigzag) transposition cipher.")

(defn- rail-of
  [position lowest]
  (let [n (mod position (* lowest 2))]
    (min n (- (* lowest 2) n))))

(defn encrypt
  "Encrypts by writing zigzag rows and reading row by row."
  [input-string key]
  (when (<= key 0)
    (throw (IllegalArgumentException. "Height of grid can't be 0 or negative")))
  (if (or (= 1 key) (<= (count input-string) key))
    input-string
    (let [lowest (dec key)
          grid (reduce (fn [g [pos ch]]
                         (update g (rail-of pos lowest) conj ch))
                       (vec (repeat key []))
                       (map-indexed vector input-string))]
      (apply str (mapcat seq grid)))))

(defn decrypt
  "Decrypts rail fence ciphertext."
  [input-string key]
  (when (<= key 0)
    (throw (IllegalArgumentException. "Height of grid can't be 0 or negative")))
  (if (= 1 key)
    input-string
    (let [lowest (dec key)
          template (reduce (fn [g pos]
                             (update g (rail-of pos lowest) conj \*))
                           (vec (repeat key []))
                           (range (count input-string)))
          grid (loop [rows template idx 0 acc []]
                 (if (empty? rows)
                   acc
                   (let [n (count (first rows))]
                     (recur (rest rows) (+ idx n)
                            (conj acc (vec (subs input-string idx (+ idx n))))))))
          ptrs (atom (vec (repeat key 0)))]
      (apply str (map (fn [pos]
                        (let [r (rail-of pos lowest)
                              p (nth @ptrs r)]
                          (swap! ptrs update r inc)
                          (nth (nth grid r) p)))
                      (range (count input-string)))))))

(defn bruteforce
  "Returns decryptions for every plausible key."
  [input-string]
  (into {} (map (fn [k] [k (decrypt input-string k)])
                (range 1 (count input-string)))))
