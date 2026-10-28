(ns clojure-algorithms.ciphers.hill-cipher
  "Hill cipher over A-Z0-9 with exact integer matrix arithmetic."
  (:require [clojure.string :as str]))

(def key-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(def ^:private modulus-n 36)

(defn- gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn replace-letters
  "Maps an alphanumeric char to its 0-35 index."
  [letter]
  (.indexOf ^String key-string (str letter)))

(defn replace-digits
  "Maps a 0-35 number back to its char."
  [num]
  (nth key-string (int num)))

(defn- minor-matrix
  [m r c]
  (vec (keep-indexed (fn [i row] (when-not (= i r)
                                   (vec (keep-indexed (fn [j v] (when-not (= j c) v)) row))))
                     m)))

(defn determinant
  "Computes the exact integer determinant of a square matrix."
  [m]
  (if (= 1 (count m))
    (ffirst m)
    (reduce + (map-indexed (fn [j v]
                             (* (if (even? j) 1 -1) v
                                (determinant (minor-matrix m 0 j))))
                           (first m)))))

(defn- adjugate
  [m]
  (let [n (count m)]
    (if (= 1 n)
      [[1]]
      (mapv (fn [i] (mapv (fn [j]
                            (* (if (even? (+ i j)) 1 -1)
                               (determinant (minor-matrix m j i))))
                          (range n)))
            (range n)))))

(defn check-determinant
  "Throws unless the key determinant is coprime to 36."
  [encrypt-key]
  (let [det (mod (determinant encrypt-key) modulus-n)]
    (when-not (= 1 (gcd det modulus-n))
      (throw (IllegalArgumentException.
              (format (str "determinant modular %d of encryption key(%d) "
                           "is not co prime w.r.t %d.\nTry another key.")
                      modulus-n det modulus-n))))))

(defn make-cipher
  "Builds a cipher map from a square key matrix (validates it)."
  [encrypt-key]
  (let [k (mapv #(mapv (fn [v] (mod v modulus-n)) %) encrypt-key)]
    (check-determinant k)
    {:encrypt-key k :break-key (count encrypt-key)}))

(defn process-text
  "Upper-cases, strips non-alphanumerics, pads to a multiple of break-key."
  [cipher text]
  (let [chars (vec (filter #(>= (.indexOf ^String key-string (str %)) 0)
                           (str/upper-case text)))
        last-ch (peek chars)]
    (loop [cs chars]
      (if (zero? (mod (count cs) (:break-key cipher)))
        (apply str cs)
        (recur (conj cs last-ch))))))

(defn- mat-vec-mul
  [m v]
  (mapv (fn [row] (mod (reduce + (map * row v)) modulus-n)) m))

(defn encrypt
  "Encrypts text with the cipher."
  [cipher text]
  (let [processed (process-text cipher (str/upper-case text))
        n (:break-key cipher)]
    (apply str (mapcat (fn [i]
                         (let [batch (subs processed i (+ i n))
                               v (mapv replace-letters batch)]
                           (map replace-digits
                                (mat-vec-mul (:encrypt-key cipher) v))))
                       (range 0 (count processed) n)))))

(defn make-decrypt-key
  "Computes the modular inverse key matrix."
  [cipher]
  (let [k (:encrypt-key cipher)
        det (mod (determinant k) modulus-n)
        det-inv (first (filter #(= 1 (mod (* det %) modulus-n))
                               (range modulus-n)))]
    (mapv #(mapv (fn [v] (mod (* det-inv v) modulus-n)) %) (adjugate k))))

(defn decrypt
  "Decrypts text with the cipher."
  [cipher text]
  (let [decrypt-key (make-decrypt-key cipher)
        processed (process-text cipher (str/upper-case text))
        n (:break-key cipher)]
    (apply str (mapcat (fn [i]
                         (let [batch (subs processed i (+ i n))
                               v (mapv replace-letters batch)]
                           (map replace-digits (mat-vec-mul decrypt-key v))))
                       (range 0 (count processed) n)))))
