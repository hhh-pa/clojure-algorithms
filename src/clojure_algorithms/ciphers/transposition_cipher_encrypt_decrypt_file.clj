(ns clojure-algorithms.ciphers.transposition-cipher-encrypt-decrypt-file
  "File helpers for the route transposition cipher."
  (:require [clojure-algorithms.ciphers.transposition-cipher :as tc]
            [clojure.java.io :as io]))

(defn encrypt-file
  "Encrypts a text file with key into the output file."
  [input-file output-file key]
  (when-not (.exists (io/file input-file))
    (throw (ex-info (str "File " input-file " does not exist.") {})))
  (let [translated (tc/encrypt-message key (slurp input-file))]
    (spit output-file translated)
    translated))

(defn decrypt-file
  "Decrypts a text file with key into the output file."
  [input-file output-file key]
  (when-not (.exists (io/file input-file))
    (throw (ex-info (str "File " input-file " does not exist.") {})))
  (let [translated (tc/decrypt-message key (slurp input-file))]
    (spit output-file translated)
    translated))
