(ns clojure-algorithms.hashes.hamming-code
  "Hamming error-correcting codes: binary conversion plus an emitter
  (encode) and receptor (decode + integrity check). The Python original
  uses numpy only for power-of-two checks; this port is pure Clojure.
  See https://en.wikipedia.org/wiki/Hamming_code")

(defn- power-of-two? [x] (and (pos? x) (zero? (bit-and x (dec x)))))

(defn text-to-bits
  "Converts text to its binary string representation."
  [text]
  (let [bytes (.getBytes ^String text "UTF-8")
        bits (apply str (map #(let [s (Integer/toBinaryString (bit-and % 0xFF))]
                                (str (apply str (repeat (- 8 (count s)) "0")) s))
                             bytes))]
    bits))

(defn text-from-bits
  "Converts a binary string back to text."
  [bits]
  (let [n (BigInteger. ^String bits 2)
        raw (.toByteArray n)
        bytes (if (and (> (alength raw) 1) (zero? (aget raw 0)))
                (java.util.Arrays/copyOfRange raw 1 (alength raw))
                raw)]
    (if (zero? (alength bytes)) "\0" (String. ^bytes bytes "UTF-8"))))

(defn- template
  "Returns the parity/data slot template for size-par and data length."
  [size-par data-len]
  (loop [x 1 qtd 0 gab []]
    (if (> x (+ size-par data-len))
      gab
      (if (and (< qtd size-par) (power-of-two? x))
        (recur (inc x) (inc qtd) (conj gab :parity))
        (recur (inc x) qtd (conj gab :data))))))

(defn- bin-pos [n] (mapv #(Integer/toBinaryString %) (range 1 (inc n))))

(defn- calc-parity
  "Computes parity bits for ordered data (nils at parity slots)."
  [size-par data-ord positions]
  (mapv (fn [bp]
          (mod (count (filter true?
                              (map-indexed
                               (fn [i x]
                                 (and (some? x)
                                      (= \1 (nth (nth positions i) (- (count (nth positions i)) bp) \0))
                                      (= "1" (str x))))
                               data-ord)))
               2))
        (range 1 (inc size-par))))

(defn emitter-converter
  "Encodes data (a binary string) with size-par parity bits.
  Returns the transmitted message as a vector of \"0\"/\"1\" strings."
  [size-par data]
  (when (<= (+ size-par (count data)) (- (bit-shift-left 1 size-par) (count data) -1))
    (throw (ex-info "size of parity don't match with size of data" {})))
  (let [gab (template size-par (count data))
        data-ord (loop [slots gab di 0 acc []]
                   (if (empty? slots)
                     acc
                     (if (= :data (first slots))
                       (recur (rest slots) (inc di) (conj acc (nth data di)))
                       (recur (rest slots) di (conj acc nil)))))
        positions (bin-pos (+ size-par (count data)))
        parity (calc-parity size-par data-ord positions)]
    (loop [ords data-ord pi 0 acc []]
      (if (empty? ords)
        acc
        (if (nil? (first ords))
          (recur (rest ords) (inc pi) (conj acc (str (nth parity pi))))
          (recur (rest ords) pi (conj acc (str (first ords)))))))))

(defn receptor-converter
  "Decodes a received message; returns [data-bits integrity-ok?]."
  [size-par data]
  (let [gab (template size-par (- (count data) size-par))
        received (mapv str (seq data))
        data-out (vec (keep-indexed (fn [i slot] (when (= :data slot) (nth received i))) gab))
        parity-received (vec (keep-indexed (fn [i slot] (when (= :parity slot) (nth received i))) gab))
        gab2 (template size-par (count data-out))
        data-ord (loop [slots gab2 di 0 acc []]
                   (if (empty? slots)
                     acc
                     (if (= :data (first slots))
                       (recur (rest slots) (inc di) (conj acc (nth data-out di)))
                       (recur (rest slots) di (conj acc nil)))))
        positions (bin-pos (+ size-par (count data-out)))
        parity (mapv str (calc-parity size-par data-ord positions))]
    [data-out (= parity-received parity)]))
