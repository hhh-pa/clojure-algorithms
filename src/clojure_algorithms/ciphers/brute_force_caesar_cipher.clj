(ns clojure-algorithms.ciphers.brute-force-caesar-cipher
  "Caesar brute force over uppercase letters.")

(def ^:private alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn decrypt-candidates
  "Returns all 26 Caesar shifts of message as [key text] pairs."
  [message]
  (mapv (fn [key]
          [key (apply str (map (fn [sym]
                                 (let [i (.indexOf ^String alpha (str sym))]
                                   (if (neg? i) sym
                                       (nth alpha (mod (- i key) 26)))))
                               message))])
        (range 26)))

(defn decrypt
  "Prints every Caesar decryption of message; returns the candidates."
  [message]
  (let [cands (decrypt-candidates message)]
    (doseq [[key text] cands]
      (println (str "Decryption using Key #" key ": " text)))
    cands))
