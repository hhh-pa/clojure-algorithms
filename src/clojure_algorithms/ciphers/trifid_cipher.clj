(ns clojure-algorithms.ciphers.trifid-cipher
  "Trifid fractionating cipher."
  (:require [clojure.string :as str]))

(def test-character-to-number
  {\A "111" \B "112" \C "113" \D "121" \E "122" \F "123" \G "131"
   \H "132" \I "133" \J "211" \K "212" \L "213" \M "221" \N "222"
   \O "223" \P "231" \Q "232" \R "233" \S "311" \T "312" \U "313"
   \V "321" \W "322" \X "323" \Y "331" \Z "332" \+ "333"})

(def test-number-to-character
  (zipmap (vals test-character-to-number) (keys test-character-to-number)))

(def ^:private trigram-list
  ["111" "112" "113" "121" "122" "123" "131" "132" "133" "211" "212" "213"
   "221" "222" "223" "231" "232" "233" "311" "312" "313" "321" "322" "323"
   "331" "332" "333"])

(defn- encrypt-part
  [message-part character-to-number]
  (let [trigrams (map character-to-number message-part)]
    (str (apply str (map first trigrams))
         (apply str (map second trigrams))
         (apply str (map #(nth % 2) trigrams)))))

(defn- decrypt-part
  [message-part character-to-number]
  (let [joined (apply str (map character-to-number message-part))
        n (count message-part)]
    [(subs joined 0 n) (subs joined n (* 2 n)) (subs joined (* 2 n) (* 3 n))]))

(defn- prepare
  [message alphabet]
  (let [alphabet (str/replace (str/upper-case (str alphabet)) " " "")
        message (str/replace (str/upper-case (str message)) " " "")]
    (when-not (= 27 (count alphabet))
      (throw (ex-info "Length of alphabet has to be 27." {})))
    (when-not (every? #(>= (.indexOf ^String alphabet (str %)) 0) message)
      (throw (IllegalArgumentException.
              "Each message character has to be included in alphabet!")))
    (let [c2n (zipmap alphabet trigram-list)
          n2c (zipmap (vals c2n) (keys c2n))]
      [message alphabet c2n n2c])))

(defn encrypt-message
  "Encrypts a message with the trifid cipher."
  ([message] (encrypt-message message "ABCDEFGHIJKLMNOPQRSTUVWXYZ." 5))
  ([message alphabet period]
   (let [[message _ c2n n2c] (prepare message alphabet)
         numeric (apply str (map #(encrypt-part (apply str %) c2n)
                                 (partition period period [] message)))]
     (apply str (map #(get n2c (apply str %)) (partition 3 numeric))))))

(defn decrypt-message
  "Decrypts a trifid ciphertext."
  ([message] (decrypt-message message "ABCDEFGHIJKLMNOPQRSTUVWXYZ." 5))
  ([message alphabet period]
   (let [[message _ c2n n2c] (prepare message alphabet)
         numeric (mapcat (fn [part]
                           (let [[a b c] (decrypt-part (apply str part) c2n)]
                             (map str a b c)))
                         (partition period period [] message))]
     (apply str (map n2c numeric)))))
