(ns clojure-algorithms.ciphers.rsa-cipher
  "RSA block cipher over ASCII text."
  (:require [clojure.string :as str]))

(def default-block-size 128)
(def byte-size 256)

(defn- mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn- pow256 [i] (.pow (BigInteger/valueOf 256) (int i)))

(defn get-blocks-from-text
  "Splits message into little-endian block integers."
  ([message] (get-blocks-from-text message default-block-size))
  ([message block-size]
   (let [bytes (vec (.getBytes ^String message "US-ASCII"))]
     (mapv (fn [start]
             (reduce + (map-indexed
                        (fn [k i] (* (bit-and (long (nth bytes i)) 0xff)
                                     (pow256 (mod i block-size))))
                        (range start (min (+ start block-size) (count bytes))))))
           (range 0 (count bytes) block-size)))))

(defn get-text-from-blocks
  "Reassembles text from block integers."
  ([block-ints message-length]
   (get-text-from-blocks block-ints message-length default-block-size))
  ([block-ints message-length block-size]
   (let [message (atom [])]
     (doseq [block block-ints]
       (let [block-message
             (reduce (fn [acc i]
                       (if (< (+ (count @message) i) message-length)
                         (let [ascii-number (quot block (pow256 i))]
                           (conj acc [(mod ascii-number byte-size)]))
                         acc))
                     [] (range (dec block-size) -1 -1))]
         (swap! message into (reverse (map first block-message)))))
     (apply str (map char @message)))))

(defn encrypt-message
  "Encrypts message blocks with public key [n e]."
  ([message key] (encrypt-message message key default-block-size))
  ([message [n e] block-size]
   (mapv #(mod-pow % e n) (get-blocks-from-text message block-size))))

(defn decrypt-message
  "Decrypts block integers with private key [n d]."
  ([encrypted-blocks message-length key]
   (decrypt-message encrypted-blocks message-length key default-block-size))
  ([encrypted-blocks message-length [n d] block-size]
   (get-text-from-blocks (mapv #(mod-pow % d n) encrypted-blocks)
                         message-length block-size)))

(defn read-key-file
  "Reads [key-size n e-or-d] from a key file."
  [key-filename]
  (let [[ks n e] (str/split (slurp key-filename) #",")]
    [(Long/parseLong ks) (bigint n) (bigint e)]))

(defn encrypt-and-write-to-file
  "Encrypts message with the key file and writes the ciphertext file."
  ([message-filename key-filename message]
   (encrypt-and-write-to-file message-filename key-filename message default-block-size))
  ([message-filename key-filename message block-size]
   (let [[key-size n e] (read-key-file key-filename)]
     (when (< key-size (* block-size 8))
       (throw (ex-info "Block size exceeds key size." {})))
     (let [content (str (count message) "_" block-size "_"
                        (str/join "," (encrypt-message message [n e] block-size)))]
       (spit message-filename content)
       content))))

(defn read-from-file-and-decrypt
  "Reads a ciphertext file and decrypts it with the key file."
  [message-filename key-filename]
  (let [[key-size n d] (read-key-file key-filename)
        [_ mlen bsize enc] (re-find #"(\d+)_(\d+)_(.*)" (slurp message-filename))
        message-length (Long/parseLong mlen)
        block-size (Long/parseLong bsize)]
    (when (< key-size (* block-size 8))
      (throw (ex-info "Block size exceeds key size." {})))
    (decrypt-message (mapv bigint (str/split enc #",")) message-length [n d] block-size)))
