(ns clojure-algorithms.ciphers.porta-cipher
  "Porta polyalphabetic cipher."
  (:require [clojure.string :as str]))

(def alphabet
  {\A ["ABCDEFGHIJKLM" "NOPQRSTUVWXYZ"] \B ["ABCDEFGHIJKLM" "NOPQRSTUVWXYZ"]
   \C ["ABCDEFGHIJKLM" "ZNOPQRSTUVWXY"] \D ["ABCDEFGHIJKLM" "ZNOPQRSTUVWXY"]
   \E ["ABCDEFGHIJKLM" "YZNOPQRSTUVWX"] \F ["ABCDEFGHIJKLM" "YZNOPQRSTUVWX"]
   \G ["ABCDEFGHIJKLM" "XYZNOPQRSTUVW"] \H ["ABCDEFGHIJKLM" "XYZNOPQRSTUVW"]
   \I ["ABCDEFGHIJKLM" "WXYZNOPQRSTUV"] \J ["ABCDEFGHIJKLM" "WXYZNOPQRSTUV"]
   \K ["ABCDEFGHIJKLM" "VWXYZNOPQRSTU"] \L ["ABCDEFGHIJKLM" "VWXYZNOPQRSTU"]
   \M ["ABCDEFGHIJKLM" "UVWXYZNOPQRST"] \N ["ABCDEFGHIJKLM" "UVWXYZNOPQRST"]
   \O ["ABCDEFGHIJKLM" "TUVWXYZNOPQRS"] \P ["ABCDEFGHIJKLM" "TUVWXYZNOPQRS"]
   \Q ["ABCDEFGHIJKLM" "STUVWXYZNOPQR"] \R ["ABCDEFGHIJKLM" "STUVWXYZNOPQR"]
   \S ["ABCDEFGHIJKLM" "RSTUVWXYZNOPQ"] \T ["ABCDEFGHIJKLM" "RSTUVWXYZNOPQ"]
   \U ["ABCDEFGHIJKLM" "QRSTUVWXYZNOP"] \V ["ABCDEFGHIJKLM" "QRSTUVWXYZNOP"]
   \W ["ABCDEFGHIJKLM" "PQRSTUVWXYZNO"] \X ["ABCDEFGHIJKLM" "PQRSTUVWXYZNO"]
   \Y ["ABCDEFGHIJKLM" "OPQRSTUVWXYZN"] \Z ["ABCDEFGHIJKLM" "OPQRSTUVWXYZN"]})

(defn generate-table
  "Expands a key into per-position alphabets."
  [key]
  (mapv alphabet (str/upper-case key)))

(defn get-position
  "Returns [row col] of char within a table pair."
  [table ch]
  (let [row (if (>= (.indexOf ^String (first table) (str ch)) 0) 0 1)]
    [row (.indexOf ^String (nth table row) (str ch))]))

(defn get-opponent
  "Returns the char opposite to ch within a table pair."
  [table ch]
  (let [[row col] (get-position table (Character/toUpperCase ^char ch))]
    (if (= 1 row)
      (nth (first table) col)
      (nth (second table) col))))

(defn encrypt
  "Encrypts words with the Porta cipher."
  [key words]
  (let [table (generate-table key)]
    (apply str (map-indexed (fn [i ch]
                              (get-opponent (nth table (mod i (count table))) ch))
                            (str/upper-case words)))))

(defn decrypt
  "Decrypts Porta ciphertext (same operation as encryption)."
  [key words]
  (encrypt key words))
