(ns clojure-algorithms.ciphers.morse-code
  "Morse code translation."
  (:require [clojure.string :as str]))

(def morse-code-dict
  {\A ".-" \B "-..." \C "-.-." \D "-.." \E "." \F "..-." \G "--." \H "...."
   \I ".." \J ".---" \K "-.-" \L ".-.." \M "--" \N "-." \O "---" \P ".--."
   \Q "--.-" \R ".-." \S "..." \T "-" \U "..-" \V "...-" \W ".--" \X "-..-"
   \Y "-.--" \Z "--.." \1 ".----" \2 "..---" \3 "...--" \4 "....-" \5 "....."
   \6 "-...." \7 "--..." \8 "---.." \9 "----." \0 "-----" \& ".-..." \@ ".--.-."
   \: "---..." \, "--..--" \. ".-.-.-" \' ".----." \" ".-..-." \? "..--.."
   \/ "-..-." \= "-...-" \+ ".-.-." \- "-....-" \( "-.--." \) "-.--.-"
   \! "-.-.--" \space "/"})

(def ^:private reverse-dict (zipmap (vals morse-code-dict) (keys morse-code-dict)))

(defn encrypt
  "Encodes a message to space-separated Morse code."
  [message]
  (str/join " " (map #(get morse-code-dict (Character/toUpperCase ^char %)) message)))

(defn decrypt
  "Decodes space-separated Morse code."
  [message]
  (apply str (map reverse-dict (str/split message #" "))))
