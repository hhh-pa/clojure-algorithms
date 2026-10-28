(ns clojure-algorithms.ciphers.autokey
  "Autokey (autoclave) cipher.")

(defn- lower-letter? [c] (<= 97 (int c) 122))

(defn encrypt
  "Encrypts plaintext with key, extending the key with the plaintext."
  [plaintext key]
  (when-not (string? plaintext)
    (throw (IllegalArgumentException. "plaintext must be a string")))
  (when-not (string? key)
    (throw (IllegalArgumentException. "key must be a string")))
  (when (empty? plaintext)
    (throw (IllegalArgumentException. "plaintext is empty")))
  (when (empty? key)
    (throw (IllegalArgumentException. "key is empty")))
  (let [plain (clojure.string/lower-case plaintext)
        full-key (clojure.string/lower-case (str key plaintext))]
    (loop [pi 0 ki 0 out []]
      (if (>= pi (count plain))
        (apply str out)
        (let [p (nth plain pi)]
          (cond
            (not (lower-letter? p))
            (recur (inc pi) ki (conj out p))
            (not (lower-letter? (nth full-key ki)))
            (recur pi (inc ki) out)
            :else
            (recur (inc pi) (inc ki)
                   (conj out (char (+ 97 (mod (+ (- (int p) 97)
                                                 (- (int (nth full-key ki)) 97))
                                              26)))))))))))

(defn decrypt
  "Decrypts ciphertext with key, rebuilding the key as it goes."
  [ciphertext key]
  (when-not (string? ciphertext)
    (throw (IllegalArgumentException. "ciphertext must be a string")))
  (when-not (string? key)
    (throw (IllegalArgumentException. "key must be a string")))
  (when (empty? ciphertext)
    (throw (IllegalArgumentException. "ciphertext is empty")))
  (when (empty? key)
    (throw (IllegalArgumentException. "key is empty")))
  (loop [ci 0 ki 0 k (vec (clojure.string/lower-case key)) out []]
    (if (>= ci (count ciphertext))
      (apply str out)
      (let [c (nth ciphertext ci)]
        (if (not (lower-letter? c))
          (recur (inc ci) ki k (conj out c))
          (let [p (char (+ 97 (mod (- (int c) (int (nth k ki))) 26)))]
            (recur (inc ci) (inc ki) (conj k p) (conj out p))))))))
