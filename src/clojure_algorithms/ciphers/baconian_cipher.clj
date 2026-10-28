(ns clojure-algorithms.ciphers.baconian-cipher
  "Bacon's cipher with A/B 5-groups."
  (:require [clojure.string :as str]))

(def encode-dict
  {\a "AAAAA" \b "AAAAB" \c "AAABA" \d "AAABB" \e "AABAA" \f "AABAB"
   \g "AABBA" \h "AABBB" \i "ABAAA" \j "BBBAA" \k "ABAAB" \l "ABABA"
   \m "ABABB" \n "ABBAA" \o "ABBAB" \p "ABBBA" \q "ABBBB" \r "BAAAA"
   \s "BAAAB" \t "BAABA" \u "BAABB" \v "BBBAB" \w "BABAA" \x "BABAB"
   \y "BABBA" \z "BABBB" \space " "})

(def decode-dict (zipmap (vals encode-dict) (keys encode-dict)))

(defn encode
  "Encodes letters and spaces to Baconian groups."
  [word]
  (apply str (map (fn [ch]
                    (let [c (Character/toLowerCase ^char ch)]
                      (if (or (Character/isLetter c) (= c \space))
                        (encode-dict c)
                        (throw (ex-info "encode() accepts only letters of the alphabet and spaces"
                                        {:char ch})))))
                  word)))

(defn decode
  "Decodes Baconian groups back to lowercase text."
  [coded]
  (when-not (every? #{\A \B \space} coded)
    (throw (ex-info "decode() accepts only 'A', 'B' and spaces" {})))
  (->> (str/split coded #" ")
       (map (fn [word]
              (apply str (map decode-dict
                              (map (partial apply str) (partition 5 word))))))
       (str/join " ")))
