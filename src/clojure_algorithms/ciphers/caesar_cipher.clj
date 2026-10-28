(ns clojure-algorithms.ciphers.caesar-cipher
  "Caesar cipher with configurable alphabet.")

(def ^:private default-alpha
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn encrypt
  "Shifts every alphabet character of input-string by key."
  ([input-string key] (encrypt input-string key nil))
  ([input-string key alphabet]
   (let [alpha (or alphabet default-alpha)]
     (apply str (map (fn [ch]
                       (let [i (.indexOf ^String alpha (str ch))]
                         (if (neg? i) ch
                             (nth alpha (mod (+ i key) (count alpha))))))
                     input-string)))))

(defn decrypt
  "Reverses a Caesar shift of input-string by key."
  ([input-string key] (decrypt input-string key nil))
  ([input-string key alphabet]
   (encrypt input-string (- key) alphabet)))

(defn brute-force
  "Returns a map of every key to its decryption."
  ([input-string] (brute-force input-string nil))
  ([input-string alphabet]
   (let [alpha (or alphabet default-alpha)]
     (into {} (map (fn [key] [key (decrypt input-string key alpha)])
                   (range 1 (inc (count alpha))))))))
