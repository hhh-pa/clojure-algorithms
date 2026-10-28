(ns clojure-algorithms.ciphers.a1z26
  "A1Z26 letter-to-number cipher.")

(defn encode
  "Encodes a lowercase alpha string to 1-based positions."
  [plain]
  (when-not (and (string? plain) (re-matches #"[a-z]+" plain))
    (throw (IllegalArgumentException.
            "plain must contain only lowercase letters (a-z)")))
  (mapv #(- (int %) 96) plain))

(defn decode
  "Decodes 1-based positions back to a lowercase string."
  [encoded]
  (apply str (map #(char (+ % 96)) encoded)))
