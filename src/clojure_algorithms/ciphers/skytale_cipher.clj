(ns clojure-algorithms.ciphers.skytale-cipher
  "Scytale (skytale) transposition cipher.")

(defn encrypt
  "Encrypts by reading every key-th char from each row offset."
  [plaintext key]
  (when-not (and (integer? key) (pos? key))
    (throw (IllegalArgumentException. "Key must be a positive integer")))
  (if (or (= 1 key) (<= (count plaintext) key))
    plaintext
    (apply str (for [row (range key)
                     i (range row (count plaintext) key)]
                 (nth plaintext i)))))

(defn decrypt
  "Decrypts skytale ciphertext."
  [ciphertext key]
  (when-not (and (integer? key) (pos? key))
    (throw (IllegalArgumentException. "Key must be a positive integer")))
  (if (or (= 1 key) (<= (count ciphertext) key))
    ciphertext
    (let [length (count ciphertext)
          base (quot length key)
          extra (mod length key)
          row-lengths (mapv #(if (< % extra) (inc base) base) (range key))
          rows (loop [rs [] idx 0 ls row-lengths]
                 (if (empty? ls)
                   rs
                   (recur (conj rs (subs ciphertext idx (+ idx (first ls))))
                          (+ idx (first ls)) (rest ls))))
          ptrs (atom (vec (repeat key 0)))]
      (apply str (for [i (range length)
                       :let [r (mod i key)
                             p (nth @ptrs r)
                             row (nth rows r)]
                       :when (< p (count row))]
                   (do (swap! ptrs update r inc)
                       (nth row p)))))))
