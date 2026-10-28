(ns clojure-algorithms.ciphers.fractionated-morse-cipher
  "Fractionated Morse cipher."
  (:require [clojure.string :as str]))

(def morse-code-dict
  {\A ".-" \B "-..." \C "-.-." \D "-.." \E "." \F "..-." \G "--." \H "...."
   \I ".." \J ".---" \K "-.-" \L ".-.." \M "--" \N "-." \O "---" \P ".--."
   \Q "--.-" \R ".-." \S "..." \T "-" \U "..-" \V "...-" \W ".--" \X "-..-"
   \Y "-.--" \Z "--.." \space ""})

(def morse-combinations
  ["..." "..-" "..x" ".-." ".--" ".-x" ".x." ".x-" ".xx" "-.." "-.-" "-.x"
   "--." "---" "--x" "-x." "-x-" "-xx" "x.." "x.-" "x.x" "x-." "x--" "x-x"
   "xx." "xx-" "xxx"])

(def ^:private reverse-dict (zipmap (vals morse-code-dict) (keys morse-code-dict)))

(defn encode-to-morse
  "Encodes plaintext to x-separated Morse code."
  [plaintext]
  (str/join "x" (map #(get morse-code-dict (Character/toUpperCase ^char %) "") plaintext)))

(defn- full-key
  [key]
  (let [k (str (str/upper-case key) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]
    (apply str (distinct k))))

(defn encrypt-fractionated-morse
  "Encrypts plaintext with the Fractionated Morse cipher."
  [plaintext key]
  (let [morse (encode-to-morse plaintext)
        k (full-key key)
        padded (str morse (apply str (repeat (- 3 (mod (count morse) 3)) \x)))
        table (assoc (zipmap morse-combinations k) "xxx" "")]
    (apply str (map #(get table (apply str %))
                    (partition 3 padded)))))

(defn decrypt-fractionated-morse
  "Decrypts Fractionated Morse ciphertext."
  [ciphertext key]
  (let [k (full-key key)
        table (zipmap k morse-combinations)
        morse (apply str (map #(get table % "") ciphertext))]
    (str/trim (apply str (map reverse-dict (str/split morse #"x" -1))))))
