(ns clojure-algorithms.ciphers.transposition-cipher
  "Route transposition cipher.")

(defn encrypt-message
  "Encrypts by reading columns of a key-wide grid."
  [key message]
  (apply str (mapcat (fn [col]
                       (map #(nth message %) (range col (count message) key)))
                     (range key))))

(defn decrypt-message
  "Decrypts route transposition ciphertext."
  [key message]
  (let [num-cols (long (Math/ceil (/ (count message) (double key))))
        num-rows key
        shaded (- (* num-cols num-rows) (count message))]
    (loop [chars (seq message) col 0 row 0 acc (vec (repeat num-cols ""))]
      (if (empty? chars)
        (apply str acc)
        (let [acc (update acc col str (first chars))
              col (inc col)]
          (if (or (= col num-cols)
                  (and (= col (dec num-cols)) (>= row (- num-rows shaded))))
            (recur (rest chars) 0 (inc row) acc)
            (recur (rest chars) col row acc)))))))
