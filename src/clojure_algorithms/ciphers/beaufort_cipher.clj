(ns clojure-algorithms.ciphers.beaufort-cipher
  "Beaufort cipher.")

(def ^:private alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def ^:private to-num (zipmap alpha (range)))
(def ^:private to-char (vec alpha))

(defn generate-key
  "Repeats key cyclically to the message length."
  [message key]
  (apply str (take (count message) (cycle key))))

(defn cipher-text
  "Encrypts an uppercase message with the expanded key."
  [message key-new]
  (let [sb (StringBuilder.)]
    (loop [chars (seq message) i 0]
      (if (empty? chars)
        (str sb)
        (let [letter (first chars)]
          (if (= letter \space)
            (do (.append sb \space) (recur (rest chars) i))
            (do (.append sb (nth to-char (mod (- (to-num letter)
                                                 (to-num (nth key-new i)))
                                              26)))
                (recur (rest chars) (inc i)))))))))

(defn original-text
  "Decrypts Beaufort ciphertext with the expanded key."
  [cipher key-new]
  (let [sb (StringBuilder.)]
    (loop [chars (seq cipher) i 0]
      (if (empty? chars)
        (str sb)
        (let [letter (first chars)]
          (if (= letter \space)
            (do (.append sb \space) (recur (rest chars) i))
            (do (.append sb (nth to-char (mod (+ (to-num letter)
                                                 (to-num (nth key-new i)) 26)
                                              26)))
                (recur (rest chars) (inc i)))))))))
