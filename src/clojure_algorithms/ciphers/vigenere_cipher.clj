(ns clojure-algorithms.ciphers.vigenere-cipher
  "Vigenere cipher."
  (:require [clojure.string :as str]))

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn translate-message
  "Translates message with key in encrypt/decrypt mode."
  [key message mode]
  (let [key (str/upper-case key)]
    (loop [chars (seq message) ki 0 out []]
      (if (empty? chars)
        (apply str out)
        (let [sym (first chars)
              num (.indexOf ^String letters (str (str/upper-case (str sym))))]
          (if (neg? num)
            (recur (rest chars) ki (conj out sym))
            (let [delta (.indexOf ^String letters (str (nth key ki)))
                  n (mod ((if (= mode "encrypt") + -) num delta) (count letters))
                  c (nth letters n)]
              (recur (rest chars) (mod (inc ki) (count key))
                     (conj out (if (Character/isUpperCase ^char sym)
                                 c (Character/toLowerCase ^char c)))))))))))

(defn encrypt-message
  "Encrypts message with key."
  [key message]
  (translate-message key message "encrypt"))

(defn decrypt-message
  "Decrypts message with key."
  [key message]
  (translate-message key message "decrypt"))
