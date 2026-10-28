(ns clojure-algorithms.ciphers.enigma-machine2
  "Enigma machine emulation with three rotors and a plugboard."
  (:require [clojure.string :as str]))

(def abc "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def rotor1 "EGZWVONAHDCLFQMSIPJBYUKXTR")
(def rotor2 "FOBHMDKEXQNRAULPGSJVTYICZW")
(def rotor3 "ZJXESIUQLHAVRMDOYGTNFWPBKC")
(def rotor4 "RMDJXFUWGISLHVTCQNKYPBEZOA")
(def rotor5 "SGLCPQWZHKXAREONTFBVIYJUDM")
(def rotor6 "HVSICLTYKQUBXDWAJZOMFGPREN")
(def rotor7 "RZWQHFMVDBKICJLNTUXAGYPSOE")
(def rotor8 "LFKIJODBEGAMQPXVUHYSTCZRWN")
(def rotor9 "KOAEGVDHXPQZMLFTYWJNBRCIUS")

(def reflector
  {\A \N \N \A \B \O \O \B \C \P \P \C \D \Q \Q \D \E \R \R \E
   \F \S \S \F \G \T \T \G \H \U \U \H \I \V \V \I \J \W \W \J
   \K \X \X \K \L \Y \Y \L \M \Z \Z \M})

(defn plugboard
  "Builds the plugboard swap map from a pairing string."
  [pbstring]
  (when-not (string? pbstring)
    (throw (IllegalArgumentException. "Plugboard setting isn't type string")))
  (when (odd? (count pbstring))
    (throw (ex-info (str "Odd number of symbols (" (count pbstring) ")") {})))
  (if (empty? pbstring)
    {}
    (let [s (str/replace pbstring " " "")]
      (when-not (every? #(>= (.indexOf ^String abc (str %)) 0) s)
        (throw (ex-info "Symbol not in list of symbols" {})))
      (when-not (= (count s) (count (set s)))
        (throw (ex-info "Duplicate symbol" {})))
      (into {} (mapcat (fn [[a b]] [[a b] [b a]]) (partition 2 s))))))

(defn- validate
  [rotpos rotsel pb]
  (when (< (count (set rotsel)) 3)
    (throw (ex-info (str "Please use 3 unique rotors (not "
                         (count (set rotsel)) ")") {})))
  (doseq [[n p] (map vector ["First" "Second" "Third"] rotpos)]
    (when-not (< 0 p 27)
      (throw (IllegalArgumentException.
              (str n " rotor position is not within range of 1..26 (" p ")")))))
  [rotpos rotsel (plugboard (str/upper-case pb))])

(defn enigma
  "Encrypts/decrypts text with the given rotor positions, rotors, plugboard."
  ([text rotor-position] (enigma text rotor-position [rotor1 rotor2 rotor3] ""))
  ([text rotor-position rotor-selection plugb]
   (let [text (str/upper-case text)
         [_ _ pb] (validate rotor-position rotor-selection plugb)
         [rp1 rp2 rp3] (map dec rotor-position)
         [rt1 rt2 rt3] rotor-selection]
     (loop [chars (seq text) p1 rp1 p2 rp2 p3 rp3 out []]
       (if (empty? chars)
         (apply str out)
         (let [sym (first chars)]
           (if (neg? (.indexOf ^String abc (str sym)))
             (recur (rest chars) p1 p2 p3 (conj out sym))
             (let [s1 (get pb sym sym)
                   s2 (nth rt1 (mod (+ (.indexOf ^String abc (str s1)) p1) 26))
                   s3 (nth rt2 (mod (+ (.indexOf ^String abc (str s2)) p2) 26))
                   s4 (nth rt3 (mod (+ (.indexOf ^String abc (str s3)) p3) 26))
                   s5 (reflector s4)
                   s6 (nth abc (mod (- (.indexOf ^String rt3 (str s5)) p3) 26))
                   s7 (nth abc (mod (- (.indexOf ^String rt2 (str s6)) p2) 26))
                   s8 (nth abc (mod (- (.indexOf ^String rt1 (str s7)) p1) 26))
                   s9 (get pb s8 s8)
                   np1 (inc p1)
                   [np1 np2] (if (>= np1 26) [0 (inc p2)] [np1 p2])
                   [np2 np3] (if (>= np2 26) [0 (inc p3)] [np2 p3])
                   np3 (if (>= np3 26) 0 np3)]
               (recur (rest chars) np1 np2 np3 (conj out s9))))))))))
