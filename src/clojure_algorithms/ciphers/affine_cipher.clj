(ns clojure-algorithms.ciphers.affine-cipher
  "Affine cipher over a printable symbol set."
  (:require [clojure-algorithms.ciphers.cryptomath-module :as cryptomath]))

(def symbols
  (str " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
       "abcdefghijklmnopqrstuvwxyz{|}~"))

(defn- gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn check-keys
  "Validates affine keys, throwing on weak or invalid keys."
  [key-a key-b mode]
  (when (and (= mode "encrypt") (= key-a 1))
    (throw (ex-info "The affine cipher becomes weak when key A is set to 1."
                    {:key-a key-a})))
  (when (and (= mode "encrypt") (zero? key-b))
    (throw (ex-info "The affine cipher becomes weak when key B is set to 0."
                    {:key-b key-b})))
  (when (or (neg? key-a) (neg? key-b) (> key-b (dec (count symbols))))
    (throw (ex-info (str "Key A must be greater than 0 and key B must be between 0 and "
                         (dec (count symbols)) ".")
                    {:key-a key-a :key-b key-b})))
  (when (not= 1 (gcd key-a (count symbols)))
    (throw (ex-info (str "Key A " key-a " and the symbol set size "
                         (count symbols) " are not relatively prime.")
                    {:key-a key-a}))))

(defn encrypt-message
  "Encrypts message with the combined integer key."
  [key message]
  (let [[key-a key-b] ((juxt quot rem) key (count symbols))]
    (check-keys key-a key-b "encrypt")
    (apply str (map (fn [sym]
                      (let [i (.indexOf ^String symbols (str sym))]
                        (if (neg? i)
                          sym
                          (nth symbols (mod (+ (* i key-a) key-b)
                                            (count symbols))))))
                    message))))

(defn decrypt-message
  "Decrypts message with the combined integer key."
  [key message]
  (let [[key-a key-b] ((juxt quot rem) key (count symbols))]
    (check-keys key-a key-b "decrypt")
    (let [inv (cryptomath/find-mod-inverse key-a (count symbols))]
      (apply str (map (fn [sym]
                        (let [i (.indexOf ^String symbols (str sym))]
                          (if (neg? i)
                            sym
                            (nth symbols (mod (* (- i key-b) inv)
                                              (count symbols))))))
                      message)))))

(defn get-random-key
  "Generates a random valid combined key."
  []
  (loop []
    (let [k (inc (rand-int (inc (count symbols))))]
      (if (and (= 1 (gcd k (count symbols)))
               (not (zero? (mod k (count symbols)))))
        (+ (* k (count symbols)) k)
        (recur)))))
